collection_by_category = function(
                collection,
                games) {
        
        mechanics = 
                collection |>
                inner_join(games,
                           by = join_by(game_id, type)) |>
                bggUtils:::unnest_mechanics()
        
        designers = 
                collection |>
                inner_join(games,
                           by = join_by(game_id, type)) |>
                bggUtils:::unnest_designers()
        
        publishers = 
                collection |>
                inner_join(games,
                           by = join_by(game_id, type)) |>
                bggUtils:::unnest_publishers() |>
                filter(id %in% bggUtils:::publishers_allow_list())
        
        artists = 
                collection |>
                inner_join(games,
                           by = join_by(game_id, type)) |>
                bggUtils:::unnest_artists()
        
        themes = 
                collection |>
                inner_join(games,
                           by = join_by(game_id, type)) |>
                bggUtils:::unnest_themes() |>
                mutate(type = 'theme',
                       value = gsub("^Theme:", "", value))
        
        components = 
                collection |>
                inner_join(games,
                           by = join_by(game_id, type)) |>
                bggUtils:::unnest_components() |>
                mutate(type = 'component',
                       value = gsub("^Component:", "", value))
        
        bind_rows(
                designers,
                publishers,
                artists,
                mechanics,
                themes,
                components) |>
                mutate(type = factor(type,
                                     levels = c("artist", "designer", "publisher", "mechanic", "component", "theme"))
                )
}

plot_collection_by_category = function(data,
                                       top_n = 15,
                                       width = 30) {
        
        data |>
                mutate(value = stringr::str_trunc(value, width = width)) |>
                group_by(type, value) |>
                count(sort = T) |>
                group_by(type) |>
                slice_max(order_by = n, n = 15, with_ties = F) |>
                ggplot(
                        aes(x = n,
                            fill = type,
                            y = tidytext::reorder_within(value, n, type))
                )+
                geom_col()+
                facet_wrap(~type,
                           scales = "free",
                           ncol = 2)+
                tidytext::scale_y_reordered()+
                scale_x_continuous(
                        breaks = scales::pretty_breaks()
                )+
                theme_bgg()+
                ylab("")+
                xlab("games")+
                ggthemes::scale_fill_few()+
                guides(fill = 'none')
        
}

upset_plot_by_category = function(data,
                                  category = 'mechanics',
                                  nintersects = 20,
                                  nsets = 15,
                                  order.by = "freq",
                                  mb.ratio = c(0.35, 0.65),
                                  ...) {
        
        sep = 
                data |>
                filter(own == 'yes') |>
                select(username, game_id, name, {{category}}) |>
                separate_longer_delim(cols = {{category}}, delim = ", ") |>
                na.omit()
        
        longer = sep |>
                mutate(value = 1) |>
                pivot_wider(
                        names_from = {{category}},
                        names_prefix = paste({{category}}, "_"),
                        values_from = value,
                        values_fill = 0
                )
        
        df =
                longer |>
                select(-username, -game_id, -name) |>
                as.data.frame()
        
        names(df) = bggUtils::present_bgg_text(names(df))
        
        df |>
                UpSetR::upset(nintersects = nintersects,
                              nsets = nsets,
                              order.by = order.by,
                              mb.ratio = mb.ratio,
                              ...)
        
}


plot_collection_by_year = function(data, min_year = 1990) {
        
        tmp =
                data |>
                mutate(yearpublished = case_when(yearpublished < min_year ~ min_year-1,
                                                 TRUE ~ yearpublished)) |>
                filter(own == 'yes')
        
        counts = 
                tmp |>
                group_by(username, yearpublished) |>
                count() |>
                ungroup()
        
        count_min_year = 
                tmp |>
                filter(yearpublished == min_year -1) |>
                group_by(username, yearpublished) |>
                count() |>
                pull(n)
        
        counts |>
                ggplot(aes(x=yearpublished, y=n))+
                geom_col()+
                bggUtils::theme_bgg()+
                labs(
                        title = "collection by year",
                        x = "yearpublished",
                        y = "games"
                )+
                annotate(
                        "text", x = 1989, y = count_min_year+ 2, 
                        size = 3,
                        label = paste("published prior \n to 1990"), 
                        hjust = 0, vjust = 1
                )
}

plot_separation = 
        function(preds) {
                
                ranks = 
                        preds |>
                        group_by(username, wflow_id, type) |>
                        arrange(desc(.pred_yes)) |>
                        mutate(rank = row_number())
                
                ranks |>
                        ggplot(aes(x=rank,
                                   y=.pred_yes))+
                        geom_point(size = 0.25,
                                   alpha = 0.25) +
                        geom_vline(data = ranks |>
                                           filter(own =='yes'),
                                   aes(xintercept = rank),
                                   color = 'deepskyblue1',
                                   alpha = 0.25
                        )+
                        bggUtils::theme_bgg() +
                        facet_wrap(~type,
                                   ncol = 1,
                                   scales = "free")
        }

safe_roc_curve = function(preds,
                          truth = own,
                          estimate = .pred_yes,
                          event_level = 'second') {
        
        preds |>
                yardstick::roc_curve({{truth}},
                                     {{estimate}},
                                     event_level = 'second')
        
}

plot_roc_curve = function(data) {
        
        data |>
                ggplot(aes(x=1-specificity,
                           color = type,
                           y=sensitivity))+
                geom_line()+
                bggUtils::theme_bgg()+
                geom_abline(slope = 1,
                            linetype = 'dotted')+
                scale_color_viridis_d()
}



# collection |>
#         prep_collection() |>
#         mutate(own_but_not_rated = case_when(own == 'yes' & is.na(rating) ~ 'yes',
#                                              TRUE ~ 'no')) |>
#         mutate(rated_but_not_own = case_when(own == 'no' & !is.na(rating) ~ 'yes',
#                                              TRUE ~ 'no')) |>
#         pivot_longer(
#                 cols = c("rated", "own", "ever_owned", "own_but_not_rated", "rated_but_not_own"),
#                 names_to = c("collection_type")
#         ) |>
#         filter(value == 'yes') |>
#         group_by(username, collection_type, value) |>
#         count(sort = T) |>
#         ungroup() |>
#         group_by(username) |>
#         select(-value) |>
#         gt::gt() |>
#         gtExtras::gt_theme_nytimes()


# collection |>
#         filter(own == 1) |>
#         collection_by_category(
#                 games = games_raw
#         ) |>
#         plot_collection_by_category()
