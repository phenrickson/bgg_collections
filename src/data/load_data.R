# what: functions for training user models

# load user colleciton
load_user_collection <- function(username) {
  # get user collection
  user_collection <- get_user_collection(username)

  user_collection
}

# # not run
# library(bggUtils)
# collection = load_user_collection(username = 'phenrickson')

# function to create outcomes and set factors for user variables
prep_collection <- function(collection, high_rating = 8, like_rating = 6) {
  # if less than 25 games ever owned, stop
  if (nrow(collection) < 25) {
    warning("low number of games; results from modeling likely unstable")
  } else if (nrow(collection) < 50) {
    warning("relatively few games in collection; results may not be reliable")
  }

  collection %>%
    mutate(
      ever_owned = case_when(
        own == 1 | prevowned == 1 ~ "yes",
        TRUE ~ "no"
      ),
      own = case_when(
        own == 1 ~ "yes",
        TRUE ~ "no"
      ),
      rated = case_when(
        !is.na(rating) ~ "yes",
        TRUE ~ "no"
      ),
      highly_rated = case_when(
        8 >= 8 ~ "yes",
        TRUE ~ "no"
      ),
      like = case_when(
        own == 1 ~ "yes",
        rating >= 6 ~ "yes",
        TRUE ~ "no"
      )
    ) |>
    mutate(
      across(
        c("own", "rated", "highly_rated", "like"),
        ~ factor(.x, levels = c("no", "yes"))
      )
    )
}

# load games
# function to retrieve games stored in gcp bucket
load_games = function(
  object_name = "raw/objects/games",
  bucket = "bgg_data",
  generation = NULL,
  ...
) {
  bggUtils::get_games_from_gcp(
    bucket = bucket,
    object = object_name,
    generation = generation
  )
}

# list of publishers to be allowed
publisher_allow_list <- function() {
  c(
    bggUtils:::publishers_allow_list(),
    25624 # add leder games
  )
}

# prepare games
# customize preprocessing from bggUtils
prepare_games <- function(
  data,
  publisher_allow = publisher_allow_list(),
  families_allow = bggUtils:::families_allow_list(),
  families_remove = bggUtils:::families_remove_list(),
  ...
) {
  data |>
    bggUtils:::preprocess_bgg_games(
      publisher_allow = publisher_allow,
      families_allow = families_allow,
      families_remove = families_remove,
      ...
    )
}

# join collection with bgg games
join_games_and_collection <- function(games, collection) {
  # take games and
  collection_and_games <-
    games |>
    left_join(
      collection,
      by = c("game_id", "name")
    )

  games_joined <-
    collection_and_games |>
    filter(!is.na(username)) |>
    nrow()

  # how many games in bgg
  print(
    glue::glue(nrow(games), " games from bgg")
  )

  # how many games joined
  print(
    glue::glue(games_joined, " games joined from collection")
  )

  collection_and_games |>
    mutate(username = unique(collection$username))
}

add_hurdle <- function(data, ratings) {
  data |>
    mutate(
      hurdle = case_when(
        usersrated >= ratings ~ "yes",
        TRUE ~ "no"
      ),
      hurdle = factor(hurdle, levels = c("no", "yes"))
    )
}

add_pred_class <- function(data, threshold) {
  data |>
    mutate(
      .pred_class = case_when(
        .pred_yes > threshold ~ "yes",
        TRUE ~ "no"
      ),
      .pred_class = factor(.pred_class, levels = c("no", "yes"))
    )
}

# estimate whether game will receive geek rating
predict_hurdle <- function(data, model, threshold, ratings = 25) {
  data |>
    add_hurdle(ratings = ratings) |>
    augment(
      new_data = _,
      model
    ) |>
    select(-.pred_class, -.pred_no) |>
    add_pred_class(threshold = threshold) |>
    rename(
      .pred_hurdle_yes = .pred_yes,
      .pred_hurdle_class = .pred_class
    )
}


# estimate averageweight
predict_averageweight <- function(data, model) {
  model |>
    augment(new_data = data) |>
    mutate(.pred = truncate_averageweight(.pred)) |>
    rename(est_averageweight = .pred)
}

# truncate
truncate_averageweight <- function(x) {
  case_when(
    x > 5 ~ 5,
    x < 1 ~ 1,
    TRUE ~ x
  )
}
