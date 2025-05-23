# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(googleCloudStorageR)

# Set the QUARTO_PATH environment variable
Sys.setenv(QUARTO_PATH = "/usr/local/bin/quarto")

# set env based on config
env <- Sys.getenv("R_CONFIG_ACTIVE", "default")
cfg <- config::get(config = env)

# Set target options:
tar_option_set(
  # global packages used in pipleine
  packages = c(
    "tibble",
    "dplyr",
    "rsample",
    "tidymodels",
    "quarto",
    "visNetwork",
    "bggUtils",
    "qs2"
  ),
  # default format for storing targets
  format = "qs",
  seed = 1999,
  memory = "transient",
  # for cloud storage
  resources = tar_resources(
    gcp = tar_resources_gcp(
      bucket = cfg$bucket,
      prefix = cfg$prefix
    )
  ),
  repository = "local"
)

# functions used in project
suppressMessages({
  tar_source("src")
})

# parameters used in the pipeline
# generation of games to load from gcp
generation <- "1745074120055107"
end_train_year <- 2023
valid_years <- 2
retrain_years <- 1
min_ratings <- 25
outcome <- "own"

# user names
users <- data.frame(
  bgg_username = c(
    "phenrickson",
    "rahdo",
    "GOBBluth89",
    "Gyges",
    "ZeeGarcia",
    "J_3MBG",
    "VWValker",
    "aboardgamebarrage"
    # ,'NellyH99'
    # ,'legotortoise'
    # ,"LupercalFR78"
    # ,'dennisflangley'
  )
)


# Replace the target list below with your own:
data <- list(
  tar_target(
    collection_model_board,
    packages = c("googleCloudStorageR"),
    command = pins::board_gcs(
      bucket = cfg$bucket,
      prefix = "model/collections/",
      versioned = T
    )
  ),
  tar_target(
    bgg_model_board,
    packages = c("googleCloudStorageR"),
    command = pins::board_gcs(
      bucket = "bgg_models",
      prefix = "dev/model/"
    )
  ),
  tar_target(
    name = games_raw,
    packages = c("googleCloudStorageR"),
    command = load_games(
      generation = cfg$generation,
      bucket = "bgg_data"
    )
  ),
  # load games for training
  tar_target(
    name = games,
    command = games_raw |>
      prepare_games()
  ),
  # # quarto report for collection analysis
  tar_target(
    name = collection_analysis,
    command = "analysis.qmd",
    format = "file"
  ),
  # load in new games
  tar_target(
    name = games_new_raw,
    command = load_games(),
    packages = c("googleCloudStorageR"),
    cue = tar_cue_age(
      name = games_new_raw,
      age = as.difftime(7, units = "days")
    )
  ),
  tar_target(
    name = games_new,
    command = {
      # load in models
      averageweight_model <- vetiver::vetiver_pin_read(
        bgg_model_board,
        name = "averageweight_bgg"
      )
      hurdle_model <- vetiver::vetiver_pin_read(
        bgg_model_board,
        name = "hurdle_bgg"
      )

      # prepare, predict, and filter based on hurdle
      games_new_raw |>
        prepare_games() |>
        predict_averageweight(
          model = averageweight_model
        ) |>
        predict_hurdle(
          model = hurdle_model,
          threshold = 0.25
        ) |>
        filter(.pred_hurdle_class == "yes")
    }
  )
)

mapped <-
  tar_map(
    values = users,
    tar_target(
      collection,
      load_user_collection(username = bgg_username),
      repository = "gcp",
    ),
    tar_target(
      model_glmnet,
      command = collection |>
        train_user_model(
          games = games,
          outcome = outcome,
          recipe = recipe_linear,
          model = glmnet_spec(),
          wflow_id = "glmnet",
          grid = glmnet_grid(),
          metrics = tune_metrics(),
          metric = "mn_log_loss",
          end_train_year = end_train_year,
          valid_years = valid_years,
          retrain_years = retrain_years,
          v = 5
        ),
      repository = "gcp"
    ),
    tar_target(
      pin_vetiver,
      model_glmnet |>
        vetiver_user_model() |>
        pin_model(board = collection_model_board)
    ),
    tar_target(
      preds,
      command = model_glmnet |>
        gather_predictions(outcome = outcome)
    ),
    tar_target(
      metrics,
      command = preds |>
        group_by(username, wflow_id, type) |>
        assess_predictions(
          metrics = tune_metrics(),
          outcome = outcome,
          event_level = "second"
        )
    ),
    tar_target(
      new_preds,
      command = {
        m <- load_model(
          board = collection_model_board,
          name = bgg_username,
          hash = pin_vetiver
        )

        m |>
          predict_user_model(
            games = games_new,
            collection = collection
          )
      }
    ),
    tar_target(
      report,
      command = {
        glue::glue(
          new_preds |>
            filter(type == "upcoming") |>
            nrow(),
          " new games for ",
          bgg_username
        )

        bgg_username |>
          render_report(
            input = collection_analysis,
            metrics = metrics,
            outcome = outcome,
            pin_vetiver = pin_vetiver,
            quiet = F
          )
      }
    )
  )

# combine objects
combined <-
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
    ),
    tar_combine(
      combined_new_preds,
      mapped[["new_preds"]],
      command = dplyr::bind_rows(!!!.x)
    )
  )

list(
  data,
  mapped,
  combined,
  # write metrics out,
  tar_target(
    name = tracking,
    command = combined_metrics |>
      pivot_wider(
        names_from = c(".metric"),
        values_from = c(".estimate")
      ) |>
      write_results(),
    format = "file"
  )
)
