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
                     "lightgbm",
                     'bonsai',
                     "glmnet",
                     "quarto",
                     "visNetwork",
                     "bggUtils"),
        # default format for storing targets
        format = "qs",
        seed = 1999,
        memory = "transient",
        garbage_collection = T
)

# functions used in project
tar_source("src/data/load_data.R")
tar_source("src/models/splitting.R")
tar_source("src/models/training.R")
tar_source("src/visualization/inference.R")
tar_source("src/visualization/tables.R")
tar_source("src/visualization/plots.R")

# parameters used in the pipeline
username = "phenrickson"
end_train_year = 2021
valid_years = 2
min_ratings = 25

# Replace the target list below with your own:
list(
        tar_target(
                name = games_raw,
                packages = c("googleCloudStorageR"),
                command = 
                        load_games(
                                object_name = "raw/objects/games",
                                generation = "1711561705858375",
                                bucket = "bgg_data"
                        )
        ),
        tar_target(
                name = games,
                command = 
                        games_raw |>
                        bggUtils::preprocess_bgg_games()
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
                name = test_data,
                command = 
                        split |>
                        assessment()
        ),
        tar_target(
                name = train_data,
                command = 
                        split |>
                        analysis() |>
                        filter(yearpublished <= end_train_year - valid_years) |>
                        filter(usersrated >= 25)
        ),
        tar_target(
                name = valid_data,
                command = 
                        split |>
                        analysis() |>
                        filter(yearpublished > end_train_year - valid_years)
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
                        train_data |>
                        create_resamples(
                                v = 5,
                                strata = own
                        )
        ),
        tar_target(
                tuned_glmnet,
                command = 
                        train_data |>
                        tune_wflow_glmnet(
                                resamples = resamples,
                                metrics = tune_metrics
                        )
        ),
        tar_target(
                tuned_lightgbm,
                command = 
                        train_data |>
                        tune_wflow_lightgbm(
                                resamples = resamples,
                                metrics = tune_metrics,
                                grid = 10
                        )
        ),
        tar_target(
                wflows,
                command =
                        as_workflow_set(
                                glmnet = tuned_glmnet,
                                lightgbm = tuned_lightgbm
                        )
        ),
        tar_target(
                name = preds_tuned_best,
                command =
                        wflows |>
                        collect_tune_preds()
        ),
        tar_target(
                best_wflow,
                command =
                        wflows |>
                        fit_best_wflow()
        ),
        tar_target(
                best_wflow_id,
                command = 
                        best_flow |>
                        pull(wflow_id)
        ),
        tar_target(
                name = preds_valid,
                command =
                        best_wflow |>
                        pluck("wflow",1) |>
                        augment(valid_data)
        ),
        tar_target(
                name = metrics_valid,
                command =
                        preds_valid |>
                        tune_metrics(own,
                                     .pred_yes,
                                     event_level = 'second')
        ),
        tar_target(
                name = final_data,
                command =
                        bind_rows(
                                train_data,
                                valid_data
                        ) |>
                        filter(usersrated >= 25)
        ),
        tar_target(
                name = final_fit,
                command =
                        best_wflow |>
                        pluck("wflow",1) |>
                        fit(final_data)
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
                        filter(yearpublished <= max(yearpublished, na.rm = T)-valid_years) |>
                        group_by(yearpublished) |>
                        tune_metrics(own, .pred_yes, event_level = 'second')
        ),
        tar_target(
                name = results,
                command =
                        metrics_valid |>
                        write_results(),
                format = "file"
        ),
        tar_target(
                name = preds_combined,
                command =
                        bind_rows(
                                preds_tuned_best |>
                                        mutate(type = 'resamples'),
                                preds_valid |>
                                        mutate(type = 'validation'),
                                preds_test |>
                                        mutate(type = 'upcoming')
                        ) |>
                        filter(yearpublished <= max(yearpublished, na.rm = T)-valid_years) |>
                        select(.pred_yes, own, game_id, name, yearpublished, type) |>
                        mutate(type = factor(type, levels = c("resamples", "validation", "upcoming")))
        ),
        tar_target(
                name = metrics_combined,
                command =
                        preds_combined |>
                        group_by(type) |>
                        tune_metrics(own, .pred_yes, event_level = 'second')
        )
        # tar_render(
        #         name = report,
        #         path = "report.qmd",
        #         output_file = "docs/report.qmd",
        #         quiet = F
        # ),
        # tar_quarto_raw(
        #         name = "user_report",
        #         quiet = F
        # )
)
