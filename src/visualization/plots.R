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
