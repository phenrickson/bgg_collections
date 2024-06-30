# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(googleCloudStorageR)

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
        memory = "transient",
        # for cloud storage
        resources = tar_resources(
                gcp = tar_resources_gcp(
                        bucket = "bgg_data",
                        prefix = 'collections'
                )
        ),
        repository = "local"
)

# functions used in project
suppressMessages({tar_source("src")})

# parameters used in the pipeline
users = data.frame(bgg_username = 
                           c(
                                   'phenrickson'
                                   ,'rahdo'
                                   ,'GOBBluth89'
                                   ,'Gyges'
                                   ,'ZeeGarcia'
                                   ,'J_3MBG'
                                   ,'VWValker'
                                   ,'aboardgamebarrage'
                                   # ,'NellyH99'
                                   # ,'legotortoise'
                                   # ,"LupercalFR78"
                                   # ,'dennisflangley'
                           )
)

#username = "phenrickson"
end_train_year = 2022
valid_years = 2
retrain_years = 1
min_ratings = 25
outcome = 'own'

# Replace the target list below with your own:
data = list(
        tar_target(
                model_board,
                command = 
                        pins::board_folder(path = "models", versioned = T)
                
        ),
        tar_target(
                name = games_raw,
                packages = c("googleCloudStorageR"),
                command = 
                        load_games(object_name = "raw/objects/games",
                                   generation = "1719427179309064",
                                   bucket = "bgg_data")
        ),
        tar_target(
                name = games,
                command = 
                        games_raw |>
                        prepare_games()
        ),
        tar_target(
                name = quarto,
                command = "analysis.qmd",
                format = "file"
        ),
        tar_target(
                name = games_new_raw,
                command = load_games(object_name = "raw/objects/games",
                                     generation = NULL,
                                     bucket = "bgg_data"),
                packages = c("googleCloudStorageR"),
                cue = tar_cue_age(
                        name = games_new_raw,
                        age = as.difftime(7, units = "days")
                )
        ),
        tar_target(
                name = games_new,
                command = games_new_raw |>
                        prepare_games()
        )
)

mapped = 
        tar_map(
                values = users,
                tar_target(
                        collection,
                        load_user_collection(username = bgg_username),
                        repository = "gcp",
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
                                                 v = 5),
                        repository = "gcp"
                ),
                # tar_target(
                #         pin_vetiver,
                #         model_glmnet |>
                #                 vetiver_user_model() |>
                #                 pin_model(board = model_board)
                # ),
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
                ),
                # tar_target(
                #         new_preds,
                #         command = 
                #                 {
                #                         m = load_model(board = model_board,
                #                                        name = username,
                #                                        hash = pin_vetiver)
                #                         
                #                         m |>
                #                                 augment(games_new)
                #                 }
                # ),
                tar_target(
                        report,
                        command =
                                bgg_username |>
                                render_report(
                                        input = quarto,
                                        metrics = metrics,
                                        outcome = outcome,
                                        quiet = F
                                )
                ),
                tar_target(
                        upload,
                        command = 
                                upload_report(file = paste0('docs/', report)),
                        packages = c('googleCloudStorageR')
                )
        )

# combine objects
combined =
        list(
                tar_combine(
                        combined_metrics,
                        mapped[["metrics"]],
                        command = dplyr::bind_rows(!!!.x)
                ),
                tar_combine(
                        combined_preds,
                        mapped[["preds"]],
                        command = dplyr::bind_rows(!!!.x)
                )
        )

list(data,
     mapped,
     combined,
     # write metrics out,
     tar_target(
             name = tracking,
             command =
                     combined_metrics |>
                     pivot_wider(names_from = c(".metric"),
                                 values_from = c(".estimate")) |>
                     write_results(),
             format = "file"
     ),
     tar_quarto(
             name = index,
             path = ".",
             quiet = F
     )
)