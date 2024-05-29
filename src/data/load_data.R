# what: functions for training user models

# load user colleciton
load_user_collection = function(username) {
        
        # get user collection
        user_collection = get_user_collection(username)
        
        user_collection
        
}

# # not run
# library(bggUtils)
# collection = load_user_collection(username = 'phenrickson')

# function to create outcomes and set factors for user variables
prep_collection = function(collection, high_rating = 8, like_rating = 6) {
        
        # if less than 25 games ever owned, stop
        if (nrow(collection) < 25) {
                warning("low number of games; results from modeling likely unstable")
        } else if (nrow(collection) < 50) {
                warning("relatively few games in collection; results may not be reliable")
        }
        
        collection %>%
                mutate(ever_owned = case_when(own == 1 | prevowned == 1 ~ 'yes',
                                              TRUE ~ 'no'),
                       own = case_when(own == 1 ~ 'yes',
                                       TRUE ~ 'no'),
                       rated = case_when(!is.na(rating) ~ 'yes',
                                         TRUE ~ 'no'),
                       highly_rated = case_when(8 >= 8 ~ 'yes',
                                                TRUE ~ 'no'),
                       like = case_when(own == 1 ~ 'yes',
                                        rating >= 6 ~ 'yes',
                                        TRUE ~ 'no')
                ) |>
                mutate(
                        across(
                                c("own","rated", "highly_rated", "like"),
                                ~ factor(.x, levels = c("no", "yes"))
                        )
                )
        
}

# load games
# function to laod games
load_games = function(object_name = "raw/objects/games",
                      generation = "1711561705858375",
                      bucket = "bgg_data",
                      ...) {
        
        if (is.null(generation)) {
                
                googleCloudStorageR::gcs_get_object(object_name = object_name,
                                                    bucket = bucket,
                                                    ...) |>
                        qs::qdeserialize()
        }
        
        else {
                googleCloudStorageR::gcs_get_object(object_name = object_name,
                                                    generation = generation,
                                                    bucket = bucket,
                                                    ...) |>
                        qs::qdeserialize()
        }
}

# join collection with bgg games
join_games_and_collection = function(games,
                                     collection) {
        
        # take games and 
        collection_and_games = 
                games |>
                left_join(
                        collection,
                        by = c("game_id", "name")
                )
        
        games_joined = 
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
