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
                     "bggUtils"),
        # default format for storing targets
        format = "qs"
)

# functions used in project
tar_source("src/data/load_data.R")
tar_source("src/models/splitting.R")

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
                        preprocess_bgg_games()
        ),
        tar_target(
                name = collection,
                command = 
                        load_user_collection(username = 'phenrickson')
        ),
        tar_target(
                name = collection_and_games,
                command = 
                        join_games_and_collection(
                                games,
                                collection |>
                                        prep_collection()
                        )
        ),
        tar_target(
                name = split,
                command = 
                        collection_and_games |>
                        split_by_year(
                                end_train_year = 2021
                        )
        ),
        tar_target(
                name = train_data,
                command = 
                        split |>
                        analysis()
        ),
        tar_target(
                name = test_data,
                command = 
                        split |>
                        assessment()
        )
)
