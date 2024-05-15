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

fit_best_wflow = function(wflow_set,
                          metric = 'mn_log_loss',
                          eval_time = NULL,
                          ...
) {
        
        
        result_1 <- extract_workflow_set_result(wflow_set, id = wflow_set$wflow_id[[1]])
        
        met_set <- tune::.get_tune_metrics(result_1)
        
        if (is.null(metric)) {
                metric <- .get_tune_metric_names(result_1)[1]
        }
        else {
                tune::check_metric_in_tune_results(tibble::as_tibble(met_set), 
                                                   metric)
        }
        if (is.null(eval_time) & tune:::is_dyn(met_set, metric)) {
                eval_time <- tune::.get_tune_eval_times(result_1)[1]
        }
        rankings <- rank_results(wflow_set, rank_metric = metric, select_best = TRUE, 
                                 eval_time = eval_time)
        best_id = rankings$wflow_id[1]
        tune_res <- workflowsets::extract_workflow_set_result(wflow_set, id = rankings$wflow_id[1])
        best_params <- tune::select_best(tune_res, metric = metric, eval_time = eval_time)
        
        fit = fit_best(tune_res, parameters = best_params, ...)
        
        tibble(
                wflow_id = best_id,
                wflow = list(fit)
        )
}

# create resamples
create_resamples = function(data,
                            v,
                            strata = own) {
        
        data |>
                vfold_cv(
                        v = v,
                        strata = own
                )
}

recipe_linear = function(data,
                         outcome = own,
                         ids = id_vars(),
                         predictors = predictor_vars()) {
        
        data |>
                build_recipe(
                        outcome = {{outcome}},
                        ids = ids,
                        predictors,
                ) |>
                add_bgg_preprocessing() |>
                add_linear_preprocessing()
}

recipe_trees = function(data,
                        outcome = own,
                        ids = id_vars(),
                        predictors = predictor_vars()) {
        
        data |>
                build_recipe(
                        outcome = {{outcome}},
                        ids = ids,
                        predictors,
                ) |>
                add_bgg_preprocessing()
}

fit_wflow = function(recipe,
                     model,
                     control = control_grid(verbose = T, save_pred = T, save_workflow =  T),
                     resamples,
                     metrics,
                     data,
                     grid = 10) {
        
        workflow() |>
                add_recipe(
                        recipe
                ) |>
                add_model(
                        model
                ) |>
                fit(
                        data
                )
}


tune_wflow = function(recipe,
                      model,
                      control = control_grid(verbose = T, save_pred = T, save_workflow =  T),
                      resamples,
                      metrics,
                      grid = 10) {
        
        workflow() |>
                add_recipe(
                        recipe
                ) |>
                add_model(
                        model
                ) |>
                tune_grid(
                        resamples = resamples,
                        grid = grid,
                        control = control,
                        metrics = metrics
                )
}

fit_wflow = function(data,
                     recipe,
                     model,
                     params) {
        
        workflow() |>
                add_recipe(
                        recipe
                ) |>
                add_model(
                        model
                ) |>
                tune_grid(
                        resamples = resamples,
                        grid = grid,
                        control = control,
                        metrics = metrics
                )
        
        
}

glmnet_grid = function(penalty = seq(-3, -0.75, length = 15),
                       mixture = c(0)) {
        
        expand.grid(
                penalty = 10^penalty,
                mixture = mixture
        )
}

glmnet_spec = function() {
        
        logistic_reg(penalty = tune::tune(),
                     mixture = tune::tune()) |>
                set_engine("glmnet")
}

lightgbm_spec = function(trees = 500) {
        
        parsnip::boost_tree(
                mode = "classification",
                trees = trees,
                min_n = tune(),
                tree_depth = tune()) |>
                set_engine("lightgbm", objective = "binary")
}

tune_wflow_glmnet = function(data,
                             resamples,
                             metrics,
                             grid = glmnet_grid()) {
        
        rec = 
                data |>
                recipe_linear()
        
        mod = 
                glmnet_spec()
        
        tuned = 
                tune_wflow(
                        recipe = rec,
                        model = mod,
                        grid = grid,
                        resamples = resamples,
                        metrics = metrics
                )
        
        return(tuned)
}

fit_wflow_glmnet = function(data,
                            params,
                            ...) {
        
        rec = 
                data |>
                recipe_linear()
        
        mod = 
                glmnet_spec()
        
        tuned = 
                tune_wflow(
                        recipe = rec,
                        model = mod,
                        grid = grid,
                        resamples = resamples,
                        metrics = metrics
                )
        
        return(tuned)
}


tune_wflow_lightgbm = function(data,
                               resamples,
                               metrics,
                               grid) {
        
        rec = 
                data |>
                recipe_trees()
        
        mod = 
                lightgbm_spec()
        
        tuned = 
                tune_wflow(
                        recipe = rec,
                        model = mod,
                        grid = grid,
                        resamples = resamples,
                        metrics = metrics
                )
        
        return(tuned)
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
                           publishers_threshold = 10,
                           designers_threshold = 10,
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

# get preds and combine with split
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

collect_best_tune_preds = function(tuned,
                                   metric = 'mn_log_loss') {
        
        best_params = 
                tuned |>
                select_best(metric = metric)
        
        tuned |> 
                get_best_preds(
                        parameters = best_params
                ) |>
                select(game_id, name, yearpublished,
                       everything())
        
        
}

collect_tune_preds = function(wflow_set) {
        
        
        wflow_set |> 
                mutate(
                        best_preds = map(result,
                                         ~ .x |> collect_best_tune_preds())
                ) |>
                select(wflow_id,
                       best_preds)
}



write_results = function(data, file = "targets-runs/results.csv") {
        
        results = data |>
                mutate_if(is.numeric, round, 4)
        
        write.csv(results, file)
}
