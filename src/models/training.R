recipe_linear = function(data,
                         outcome,
                         ids = id_vars(),
                         predictors = predictor_vars()) {
        
        data |>
                build_recipe(
                        outcome = {{outcome}},
                        ids = ids,
                        predictors = predictors
                ) |>
                add_bgg_preprocessing() |>
                add_linear_preprocessing()   
        
}

recipe_trees = function(data,
                        outcome,
                        ids = id_vars(),
                        predictors = predictor_vars()) {
        
        data |>
                build_recipe(
                        outcome = {{outcome}},
                        ids = ids,
                        predictors = predictors
                ) |>
                add_bgg_preprocessing()
        
}


lightgbm_spec = function(trees = 500, ...) {
        
        
        require(bonsai)
        
        parsnip::boost_tree(
                mode = "classification",
                trees = trees,
                min_n = tune(),
                tree_depth = tune(),
                ...) |>
                set_engine("lightgbm", 
                           objective = "binary")
}

lightgbm_grid = 
        function(size = 15) {
                
                grid_max_entropy(
                        x = dials::parameters(
                                min_n(), # 2nd important
                                tree_depth() # 3rd most important
                        ),
                        size = size
                )
        }

glmnet_spec = function() {
        
        logistic_reg(penalty = tune::tune(),
                     mixture = tune::tune()) %>%
                set_engine("glmnet")
}

glmnet_grid = function(penalty = seq(-3, -0.75, length = 10),
                       mixture = c(0)) {
        
        expand.grid(
                penalty = 10 ^ penalty,
                mixture = mixture
        )
}

tune_metrics = function() {
        
        metric_set(yardstick::mn_log_loss,
                   yardstick::roc_auc,
                   yardstick::pr_auc)
}


train_user_model = function(collection, 
                            games,
                            outcome,
                            recipe,
                            model,
                            wflow_id,
                            grid,
                            metrics = tune_metrics(),
                            metric = 'mn_log_loss',
                            end_train_year,
                            valid_years = 2,
                            retrain_years = 1,
                            min_ratings = 25,
                            v = 5) {
        
        collection |>
                prepare_user_model(games = games,
                                   outcome = {{outcome}},
                                   recipe = recipe,
                                   model = model,
                                   wflow_id = wflow_id,
                                   end_train_year = end_train_year,
                                   valid_years = valid_years,
                                   retrain_years = retrain_years,
                                   min_ratings = min_ratings,
                                   v = v) |>
                tune_user_model(grid = grid,
                                metrics = metrics,
                                metric = metric) |>
                validate_user_model() |>
                finalize_user_model()
        
}


prepare_split = function(collection, 
                         games,
                         end_train_year,
                         valid_years,
                         retrain_years) {
        
        collection_and_games = 
                join_games_and_collection(
                        games,
                        collection
                ) |>
                prep_collection()
        
        split = 
                collection_and_games |>
                split_by_year(end_train_year = end_train_year)
        
        train_data = 
                split |>
                analysis() |>
                filter(yearpublished <= end_train_year - valid_years) |>
                filter(usersrated >= min_ratings)
        
        valid_data = 
                split |>
                analysis() |>
                filter(yearpublished > end_train_year - valid_years)
        
        test_data = 
                split |>
                assessment()
        
        new_split = 
                list(
                        "train" = train_data,
                        "valid" = valid_data,
                        "test" = test_data
                )
        
        tibble(username = unique(collection$username),
               split = list(new_split),
               settings = list(tibble(end_train_year = end_train_year,
                                      valid_years = valid_years,
                                      retrain_years = retrain_years,
                                      min_ratings = min_ratings))
        )
}

prepare_wflow = function(prepared_collection,
                         recipe,
                         model,
                         outcome,
                         v = 5) {
        
        
        train_data = 
                prepared_collection |>
                pluck("split", 1) |>
                pluck("train")
        
        resamples = 
                train_data |>
                create_resamples(
                        v = v,
                        strata = {{outcome}}
                )
        
        rec = 
                train_data |>
                recipe(
                        outcome = {{outcome}}
                )
        
        wflow =
                workflow() |>
                add_recipe(
                        rec
                ) |>
                add_model(
                        model
                )
        
        prepared_collection |>
                add_column(
                        resamples = list(resamples),
                        wflow = list(wflow)
                )
        
}

tune_wflow = function(prepared_wflow,
                      grid,
                      control = control_grid(verbose = T, save_pred = T),
                      metrics = tune_metrics,
                      ...) {
        
        resamples = 
                prepared_wflow |>
                pluck("resamples", 1)
        
        wflow = 
                prepared_wflow |> 
                pluck("wflow", 1)
        
        tuned = 
                wflow |>
                tune_grid(
                        resamples = resamples,
                        grid = grid,
                        control = control,
                        metrics = metrics,
                        ...
                )
        
        prepared_wflow |>
                add_column(result = list(tuned))
}

best_params = function(tuned_wflow,
                       metric = 'mn_log_loss',
                       ...) {
        
        tuned_wflow |> 
                mutate(params = map(result, ~ .x |> select_best(metric = metric)))
}

fit_wflow = function(tuned_wflow,
                     data = c("train", "valid"),
                     valid = T) {
        
        params = 
                tuned_wflow |>
                pluck("params", 1)
        
        wflow = 
                tuned_wflow |>
                pluck("wflow", 1)
        
        settings = 
                tuned_wflow |>
                pluck("settings", 1)
        
        split = 
                tuned_wflow |>
                pluck("split", 1)
        
        dat = 
                map_df(data, ~ pluck(split, .x))
        
        if (valid==T) {
                
                dat =  dat |>
                        filter(yearpublished <= (settings$end_train_year + settings$valid_years - settings$retrain_years))
        }
        
        new =
                dat |>
                filter(usersrated >= settings$min_ratings)
        
        fit = 
                wflow |>
                finalize_workflow(parameters = params) |>
                fit(new)
        
        tuned_wflow |>
                mutate(wflow = list(fit))
}

predict_split = function(tuned_wflow,
                         data = "train") {
        
        wflow = 
                tuned_wflow |>
                pluck("wflow", 1)
        
        split = 
                tuned_wflow |>
                pluck("split", 1)
        
        dat = 
                split |>
                pluck(data)
        
        preds = 
                wflow |>
                augment(dat,
                        type = 'prob') |>
                arrange(desc(.pred_yes))
        
        return(preds)
        
}

best_tune_preds = function(tuned_wflow) {
        
        result = 
                tuned_wflow |> 
                pluck("result", 1)
        
        params =
                tuned_wflow |>
                pluck("params", 1)
        
        result |>
                get_best_preds(parameters = params) |>
                arrange(desc(.pred_yes)) |>
                mutate(type = 'resamples')
}


prepare_user_model = function(collection,
                              games,
                              outcome,
                              recipe,
                              model,
                              wflow_id,
                              end_train_year,
                              valid_years,
                              retrain_years,
                              min_ratings,
                              v = v) {
        
        tuned = 
                collection |>
                prepare_split(games = games,
                              end_train_year = end_train_year,
                              valid_years = valid_years,
                              retrain_years = retrain_years) |>
                prepare_wflow(recipe = recipe,
                              outcome = {{outcome}},
                              model = model,
                              v = v) |>
                add_column(wflow_id = wflow_id) |>
                select(username, wflow_id, everything())
        
}

tune_user_model = function(wflow,
                           grid,
                           metrics = tune_metrics(),
                           metric = 'mn_log_loss',
                           ...) {
        
        tuned = 
                wflow |>
                tune_wflow(grid = grid,
                           metrics = metrics,
                           ...) |>
                best_params(metric = metric)
        
        tune_preds = 
                tuned |>
                best_tune_preds()
        
        tuned |>
                add_column(tune_preds = list(tune_preds))
}

validate_user_model = function(tuned_wflow) {
        
        train_fit = 
                tuned_wflow |>
                fit_wflow(data = 'train')
        
        valid_preds = 
                train_fit |>
                predict_split(data = "valid") |>
                mutate(type = 'valid')
        
        train_fit |>
                add_column(valid_preds = list(valid_preds))
        
}

finalize_user_model = function(tuned_wflow) {
        
        final_fit = 
                tuned_wflow |>
                fit_wflow(data = c('train', 'valid'))
        
        test_preds =
                final_fit |>
                predict_split("test") |>
                mutate(type = 'test')
        
        final_fit |>
                add_column(test_preds = list(test_preds))
        
}

gather_predictions = function(tuned,
                              outcome = own) {
        
        tmp = 
                tuned |>
                select(any_of(c("username", "wflow_id")), ends_with("_preds"))
        
        preds = 
                tmp |>
                pivot_longer(cols = ends_with("preds"), 
                             names_to = c("set"), 
                             values_to = c("preds")) |>
                mutate(preds = map(preds, ~ .x |> select(-any_of(c("username"))))) |>
                unnest(preds)
        
        
        preds |>
                select(username, 
                       wflow_id, 
                       type, 
                       starts_with(".pred"), 
                       id, 
                       .row, 
                       {{outcome}},
                       game_id, 
                       name, 
                       yearpublished)
        
}

assess_predictions = function(preds,
                              metrics = tune_metrics(),
                              outcome = own,
                              event_level = 'second') {
        
        preds |>
                metrics(
                        {{outcome}},
                        .pred_yes,
                        event_level = 'second'
                )
}


# recipes for bgg outcomes
predictor_vars= function(vars =
                                 c("minplayers",
                                   "maxplayers",
                                   "playingtime",
                                   "minplaytime",
                                   "maxplaytime",
                                   "image",
                                   "thumbnail",
                                   "minage",
                                   "categories",
                                   "mechanics",
                                   "publishers",
                                   "designers",
                                   "artists",
                                   "families",
                                   "mechanisms",
                                   "components",
                                   "themes"
                                 )
) {vars}

id_vars = function(vars =
                           c("game_id",
                             "name",
                             "username",
                             "numweights",
                             "yearpublished",
                             "averageweight",
                             "average",
                             "usersrated",
                             "image",
                             "thumbnail",
                             "description")) {vars}

# spline_vars = function(vars = c("number_mechanics",
#                                 "number_categories")) {vars}
# 
# discrete_vars = function(vars = spline_vars()) {vars}
# 
# 
# # basic recipe setup
build_recipe = function(data,
                        outcome,
                        ids = id_vars(),
                        predictors = predictor_vars(),
                        ...) {
        
        recipe(x=data) %>%
                # set ids
                update_role(
                        any_of(ids),
                        new_role = "id"
                ) %>%
                # set predictors
                update_role(
                        any_of(predictors),
                        new_role = "predictor"
                ) %>%
                # set outcome
                update_role(
                        {{ outcome }},
                        new_role = "outcome"
                ) %>%
                # set anything else as id
                update_role(
                        -has_role("predictor"),
                        -has_role("outcome"),
                        -has_role("id"),
                        new_role = "extras"
                )
}

# create resamples
create_resamples = function(data,
                            v,
                            strata = own) {
        
        data |>
                vfold_cv(
                        v = v,
                        strata = {{strata}}
                )
}

# standard preprocessing for bgg variables
add_bgg_preprocessing = function(recipe,
                                 ...) {
        
        recipe |>
                step_rm(has_role("extras")) |>
                add_preprocessing() |>
                add_imputation() |>
                add_bgg_dummies(...)
}

add_linear_preprocessing = function(recipe) {
        
        recipe |>
                # spline for year
                add_splines(vars = "year", degree = 4) |>
                # splines with fifth degree polynomials for mechanics/categories
                add_splines(c("number_mechanics", "number_categories")) |>
                # remove zero variance
                add_zv() |>
                # filter for correlation
                step_corr(all_numeric_predictors(), threshold = 0.95) |>
                add_normalize()
}

# function for extracting dummies from nominal features
add_dummies = function(recipe,
                       variable,
                       threshold = 100) {
        
        variable = enquo(variable)
        
        recipe %>%
                # tokenize
                step_dummy_extract(!!variable,
                                   sep = ", ",
                                   other = "remove_other_field",
                                   threshold = threshold) %>%
                # remove other var
                step_rm(contains("remove_other_field"))
        
        
}

# standard dummy recipes
add_bgg_dummies = function(recipe,
                           mechanics_threshold = 10,
                           categories_threshold = 1,
                           families_threshold = 100,
                           publishers_threshold = 1,
                           designers_threshold = 5,
                           artists_threshold = 10,
                           components_threshold = 25,
                           themes_threshold = 25,
                           mechanisms_threshold = 25
) {
        
        recipe %>%
                # include most mechanics
                add_dummies(mechanics,
                            threshold = mechanics_threshold) %>%
                # include all categories
                add_dummies(categories,
                            threshold = categories_threshold) %>%
                # families
                add_dummies(families,
                            threshold = families_threshold) %>%
                # publishers
                add_dummies(publishers,
                            threshold = publishers_threshold) %>%
                # designers
                add_dummies(designers,
                            threshold = designers_threshold) %>%
                # artists
                add_dummies(artists,
                            threshold = artists_threshold) %>%
                # components
                add_dummies(components,
                            threshold = components_threshold) %>%
                # themes
                add_dummies(themes,
                            threshold = themes_threshold) %>%
                # mechanisms
                add_dummies(mechanisms,
                            threshold = mechanisms_threshold)
}


# standardized preprocessing
add_preprocessing = function(recipe) {
        
        recipe %>%
                # indicate missingness in numeric features
                step_indicate_na(all_numeric_predictors(),
                                 prefix = "missing") %>%
                # indicate missingness in image, description, or thumbnail
                step_indicate_na(image, thumbnail,
                                 prefix = "missing") %>%
                update_role(image, thumbnail,
                            new_role = "id") %>%
                # make time per player variable
                step_mutate(time_per_player = playingtime/ maxplayers) %>%
                # remove zero variance predictors
                step_zv(all_predictors()) %>%
                # number_mechanics
                step_mutate(number_mechanics =
                                    dplyr::case_when(
                                            is.na(mechanics) ~ 0,
                                            TRUE ~ stringr::str_count(mechanics, ',') + 1
                                    )
                ) %>%
                # number categories
                step_mutate(number_categories =
                                    dplyr::case_when(
                                            is.na(categories) ~ 0,
                                            TRUE ~ stringr::str_count(categories, ',') + 1
                                    )
                ) %>%
                # log time per player and playingtime
                step_log(time_per_player,
                         playingtime,
                         offset = 1) %>%
                # truncate yearpublished
                step_mutate(year = dplyr::case_when(yearpublished < 1900 ~ 1900,
                                                    TRUE ~ yearpublished),
                            role = "predictor") %>%
                # indicator for published before 1900
                step_mutate(published_before_1900 = dplyr::case_when(yearpublished < 1900 ~ 1,
                                                                     TRUE ~ 0)) %>%
                # solo game
                # big box/deluxe/anniversary edition
                step_mutate(deluxe_edition = dplyr::case_when(grepl("kickstarter|big box|deluxe|mega box", tolower(name))==T ~ 1,
                                                              TRUE ~ 0)) %>%
                # description word count
                step_mutate(word_count = stringi::stri_count_words(description)) %>%
                step_mutate(word_count = tidyr::replace_na(word_count, 0)) %>%
                # magical phrase in description
                step_mutate(description_from_publisher = dplyr::case_when(grepl("description from publisher", tolower(description))==T ~ 1,
                                                                          TRUE ~ 0))
}


# imputation
add_imputation = function(recipe) {
        
        recipe %>%
                # impute missingness in selected features with median
                step_impute_median(playingtime,
                                   minplayers,
                                   maxplayers,
                                   minage,
                                   time_per_player) %>% # medianimpute numeric predictors
                # truncate minage to no greater than 18
                step_mutate(minage = dplyr::case_when(minage > 18 ~ 18,
                                                      minage < 0 ~ 0,
                                                      TRUE ~ minage)) %>%
                # truncate player counts
                step_mutate(minplayers = dplyr::case_when(minplayers > 10 ~ 10,
                                                          TRUE ~ minplayers)) %>%
                step_mutate(maxplayers = dplyr::case_when(maxplayers > 20 ~ 10,
                                                          maxplayers <=1 ~ 1,
                                                          TRUE ~ maxplayers)) %>%
                step_rm(minplaytime, maxplaytime)
}


# normalize all numeric predictors
add_normalize = function(recipe) {
        
        recipe %>%
                step_normalize(all_numeric_predictors())
}
# 
# 
# # add pca
# add_pca = function(recipe,
#                    ...) {
#     
#     recipe %>%
#         step_pca(all_numeric_predictors(),
#                  ...)
# }
# 
# # add corr
# add_corr = function(recipe,
#                     my_threshold = 0.9) {
#     
#     recipe %>%
#         step_corr(all_numeric_predictors(),
#                   threshold = my_threshold)
#     
# }
# 
# 
# step to remove zero variance
add_zv = function(recipe) {
        
        recipe %>%
                step_zv(all_numeric_predictors())
        
}

# splines
# add splines for nonlinear effects for linear models
add_splines= function(recipe,
                      vars = c("year",
                               "number_mechanics",
                               "number_categories"),
                      degree = 5) {
        
        
        step_spline_feature = function(recipe,
                                       feature,
                                       ...) {
                
                feature = enquo(feature)
                
                recipe %>%
                        # add splines
                        step_ns(
                                !!feature,
                                deg_free = degree)
        }
        
        for (i in 1 :length(vars)) {
                
                recipe = recipe %>%
                        step_spline_feature(feature = !!vars[i])
                
        }
        
        recipe
        
        
}

get_best_preds = function(obj,
                          ...) {
        
        obj |>
                collect_predictions(...) |>
                arrange(desc(.pred_yes)) |>
                left_join(
                        obj |> 
                                pluck("splits", 1) |> 
                                pluck("data") |>
                                select(game_id, name, yearpublished) |>
                                mutate(.row = row_number()),
                        by = join_by(.row)
                )
        
}



write_results = function(data, file = "targets-runs/results.csv") {
        
        results = data |>
                mutate_if(is.numeric, round, 4)
        
        write.csv(results, file)
}
