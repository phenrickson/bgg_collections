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
                     "quarto",
                     "visNetwork",
                     "bggUtils"),
        # default format for storing targets
        format = "qs",
        seed = 1999,
        memory = "transient"
        
)

# functions used in project
tar_source("src")


# function to render quarto report and output given username
render_report = function(username,
                         input,
                         metrics,
                         ...) {
        
        
        quarto::quarto_render(
                input = input,
                execute_params = list(username = username,
                                      metrics = metrics),
                output_file = glue::glue("{username}.html")
        )
}

# parameters used in the pipeline
users = data.frame(bgg_username = 
                           c(
                                   # 'phenrickson',
                                   # 'rahdo',
                                   # 'GOBBluth89',
                                   # 'Gyges',
                                   # 'ZeeGarcia',
                                   # 'J_3MBG',
                                   # 'VWValker',
                                   # 'aboardgamebarrage',
                                   "legotortoise",
                                   'dennisflangley'
                           )
)

#username = "phenrickson"
end_train_year = 2022
valid_years = 2
retrain_years = 1
min_ratings = 25
outcome = 'like'

# Replace the target list below with your own:
data = list(
        tar_target(
                name = games_raw,
                packages = c("googleCloudStorageR"),
                command = 
                        load_games(
                                object_name = "raw/objects/games",
                                generation = "1716489185536915",
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
                name = quarto,
                command = "analysis.qmd",
                format = "file"
        )
)

mapped = 
        tar_map(
                values = users,
                tar_target(
                        collection,
                        load_user_collection(username = bgg_username)
                ),
                tar_target(
                        model_glmnet,
                        command =
                                collection |>
                                train_user_model(games = games,
                                                 outcome = outcome,
                                                 recipe = recipe_linear,
                                                 model = glmnet_spec(),
                                                 wflow_id = "glmnet",
                                                 grid = glmnet_grid(),
                                                 metrics = tune_metrics(),
                                                 metric = 'mn_log_loss',
                                                 end_train_year = end_train_year,
                                                 valid_years = valid_years,
                                                 retrain_years = retrain_years,
                                                 v = 3)
                ),
                tar_target(
                        model_lightgbm,
                        command =
                                collection |>
                                train_user_model(games = games,
                                                 outcome = outcome,
                                                 recipe = recipe_trees,
                                                 model = lightgbm_spec(),
                                                 wflow_id = "lightgbm",
                                                 grid = lightgbm_grid(),
                                                 metrics = tune_metrics(),
                                                 metric = 'mn_log_loss',
                                                 end_train_year = end_train_year,
                                                 valid_years = valid_years,
                                                 retrain_years = retrain_years,
                                                 v = 3)
                ),
                tar_target(
                        preds,
                        command = 
                                model_glmnet |>
                                gather_predictions(outcome = outcome)
                ),
                tar_target(
                        metrics,
                        command = 
                                preds |>
                                group_by(username, wflow_id, type) |>
                                assess_predictions(metrics = tune_metrics(),
                                                   outcome = outcome,
                                                   event_level = 'second')
                )
                # tar_target(
                #         report,
                #         command =
                #                 bgg_username |>
                #                 render_report(
                #                         input = quarto,
                #                         metrics = metrics,
                #                         outcome = outcome
                #                 )
                #         #   cue = tar_cue(mode = "always")
                # )
        )

list(data,
     mapped)

# # combine objects
# combined = 
#         list(
#                 tar_combine(
#                         combined_metrics,
#                         mapped[["metrics"]],
#                         command = dplyr::bind_rows(!!!.x)
#                 ),
#                 tar_combine(
#                         combined_preds,
#                         mapped[["preds"]],
#                         command = dplyr::bind_rows(!!!.x)
#                 )
#         )
# 
# list(data,
#      mapped,
#      combined
# )
#      # write metrics out,
#      tar_target(
#              name = tracking,
#              command = 
#                      combined_metrics |> 
#                      pivot_wider(names_from = c(".metric"), 
#                                  values_from = c(".estimate")) |>
#                      write_results(),
#              format = "file"
#      ),
#      tar_quarto(
#              name = index,
#              path = ".",
#              quiet = F
#      )
# )