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
        seed = 1999
)

# functions used in project
tar_source("src/data/load_data.R")
tar_source("src/models/splitting.R")
tar_source("src/models/training.R")
tar_source("src/visualization/inference.R")
tar_source("src/visualization/tables.R")
tar_source("src/visualization/plots.R")


# parameters used in the pipeline
users = data.frame(username = 'phenrickson')
#username = "phenrickson"
end_train_year = 2021
valid_years = 2
retrain_years = 1
min_ratings = 25

# Replace the target list below with your own:
list(
        tar_target(
                name = games_raw,
                packages = c("googleCloudStorageR"),
                command = 
                        load_games(
                                object_name = "raw/objects/games",
                                generation = "1715797632435985",
                                bucket = "bgg_data"
                        )
        ),
        tar_target(
                name = games,
                command = 
                        games_raw |>
                        bggUtils::preprocess_bgg_games()
        ),
        tar_map(
                values = users,
                tar_target(
                        collection,
                        load_user_collection(username = username)
                ),
                tar_target(
                        model_glmnet,
                        command =
                                collection |>
                                train_user_model(games = games,
                                                 outcome = own,
                                                 recipe = recipe_linear,
                                                 model = glmnet_spec(),
                                                 wflow_id = "glmnet",
                                                 grid = glmnet_grid(),
                                                 metrics = tune_metrics(),
                                                 metric = 'mn_log_loss',
                                                 end_train_year = end_train_year,
                                                 valid_years = valid_years,
                                                 retrain_years = retrain_years)
                ),
                tar_target(
                        metrics,
                        command = 
                                bind_rows(model_glmnet) |>
                                gather_predictions() |>
                                assess_predictions(metrics = tune_metrics())
                ),
                tar_quarto(
                        name = report,
                        path = "report"
                )
        )
)