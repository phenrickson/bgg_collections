# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
        # global packages used in pipleine
        packages = c("tibble",
                     "dplyr",
                     "rsample",
                     "tidymodels",
                     "bggUtils"),
        # default format for storing targets
        format = "qs",
        seed = 1999
)

# functions used in project
tar_source("src/data/load_data.R")
tar_source("src/models/splitting.R")
tar_source("src/models/training.R")

# configure for parallel processing
library(future)
library(future.callr)
plan(callr)

# parameters used in the workflow
username = 'phenrickson'
end_train_year = 2021
min_ratings = 25

# Replace the target list below with your own:
list(
        tar_target(
                name = games,
                packages = c("googleCloudStorageR"),
                command = 
                        load_games(
                                object_name = "raw/objects/games",
                                generation = "1711561705858375",
                                bucket = "bgg_data"
                        ) |>
                        preprocess_games()
        ),
        tar_target(
                name = collection,
                command = 
                        load_user_collection(username = username)
        ),
        tar_target(
                name = collection_and_games,
                command = 
                        join_games_and_collection(
                                games,
                                collection
                        ) |>
                        prep_collection()
        ), 
        tar_target(
                name = split,
                command = 
                        collection_and_games |>
                        split_by_year(
                                end_train_year = end_train_year
                        )
        ),
        tar_target(
                name = train_data,
                command = 
                        split |>
                        analysis() |>
                        filter(usersrated >=min_ratings)
        ),
        tar_target(
                name = test_data,
                command = 
                        split |>
                        assessment()
        ),
        tar_target(
                name = valid_split,
                command = 
                        train_data |>
                        split_by_year(
                                end_train_year = end_train_year-2
                        )
        ),
        tar_target(
                name = model_spec,
                command = 
                        logistic_reg(penalty = tune::tune(),
                                     mixture = tune::tune()) %>%
                        set_engine("glmnet")
        ),
        tar_target(
                name = recipe,
                command = 
                        valid_split |>
                        analysis() |>
                        build_recipe(
                                outcome = own,
                                ids = id_vars(),
                                predictors = predictor_vars()
                        ) |>
                        step_rm(has_role("extras")) |>
                        add_preprocessing() |>
                        add_imputation() |>
                        add_bgg_dummies() |>
                        # spline for year
                        add_splines(vars = "year", degree = 4) |>
                        # splines with fifth degree polynomials for mechanics/categories
                        add_splines(c("number_mechanics", "number_categories")) |>
                        # remove zero variance
                        add_zv() |>
                        add_normalize()
        ),
        tar_target(
                name = tuning_grid,
                command = 
                        expand.grid(
                                penalty = 10 ^ seq(-3, -0.75, length = 15),
                                mixture = c(0)
                        )
        ),
        tar_target(
                name = tune_metrics,
                command = 
                        metric_set(yardstick::mn_log_loss,
                                   yardstick::roc_auc)
        ),
        tar_target(
                name = resamples,
                command = 
                        valid_split |>
                        analysis() |>
                        vfold_cv(
                                v = 5,
                                strata = own
                        )
        ),
        tar_target(
                name = wflow,
                command = 
                        workflow() |>
                        add_recipe(
                                recipe
                        ) |>
                        add_model(
                                model_spec
                        )
        ),
        tar_target(
                name = tuned,
                command = 
                        wflow |>
                        tune_grid(
                                resamples = resamples,
                                grid = tuning_grid,
                                control = 
                                        control_grid(
                                                verbose = T,
                                                save_pred = T),
                                metrics = tune_metrics
                        )
        ),
        tar_target(
                name = plot_tuning,
                command = 
                        tuned |>
                        autoplot() +
                        theme_bw()
        ),
        tar_target(
                name = best_par,
                command = 
                        tuned |> 
                        select_best(metric = 'mn_log_loss')
        ),
        tar_target(
                name = preds_tuned_best,
                command = 
                        tuned |> 
                        collect_predictions(parameters = best_par) |> 
                        arrange(desc(.pred_yes)) |> 
                        left_join(
                                tuned |> 
                                        pluck("splits", 1) |> 
                                        pluck("data") |>
                                        select(game_id, name, yearpublished) |>
                                        mutate(.row = row_number())
                        )
        ),
        tar_target(
                name = last_fit,
                command = 
                        wflow |> 
                        finalize_workflow(parameters = best_par) |> 
                        last_fit(
                                split = valid_split,
                                metrics = tune_metrics
                        )
        ),
        tar_target(
                name = preds_valid,
                command = 
                        last_fit |> 
                        collect_predictions() |>
                        arrange(desc(.pred_yes)) |> 
                        left_join(
                                last_fit |> 
                                        pluck("splits", 1) |> 
                                        pluck("data") |>
                                        select(game_id, name, yearpublished) |>
                                        mutate(.row = row_number())
                        )
        ),
        tar_target(
                name = metrics_valid,
                command = 
                        last_fit |>
                        collect_metrics()
        ),
        tar_target(
                name = final_fit,
                command = 
                        wflow |>
                        finalize_workflow(parameters = best_par) |>
                        fit(
                                valid_split$data
                        )
        ),
        tar_target(
                name = preds_test,
                command =
                        final_fit |>
                        augment(test_data)
        ),
        tar_target(
                name = metrics_test,
                command = 
                        preds_test |> 
                        group_by(yearpublished) |> 
                        tune_metrics(own, .pred_yes, event_level = 'second')
        ),
        tar_target(
                name = results,
                command = 
                        {
                                results = 
                                        list(
                                                "valid" = metrics_valid,
                                                "test" = metrics_test
                                        ) |> 
                                        jsonlite::toJSON()
                                
                                write(results, "results.json")
                        },
                format = "file"
        )
)
