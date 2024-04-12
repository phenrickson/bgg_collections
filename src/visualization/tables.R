# functions for add linkjing
make_bgg_link = function(game_id) {
        
        paste0("https://boardgamegeek.com/boardgame/",
               game_id)
}

make_hyperlink = function(myurl,
                          mytext=myurl) {
        paste('<a href="',myurl,'">',mytext,'</a>')
}

make_web_image =
        function(x, height=100) {
                
                web_image(url = x,
                          height = px(height) 
                ) 
                
        }


prep_predictions_tbl = function(predictions, 
                                outcome = "own",
                                games = games,
                                description_length = 300) {
        
        predictions |>
                arrange(desc(.pred_yes)) |>
                select(game_id, name, yearpublished, .pred_yes, any_of(outcome)) |>
                left_join(
                        games |>
                                select(game_id, description, image, thumbnail),
                        by = join_by(game_id)
                ) |>
                # make bgg links
                mutate(link = make_bgg_link(game_id)) |>
                # sort
                mutate(
                        rank = row_number(),
                        game = name,
                        image,
                        description,
                        published = yearpublished,
                        game_id,
                        .pred_yes,
                        own,
                        link,
                        .keep = 'none') |>
                # make link
                mutate(game = map2(paste0(game, " (", published, ")"),
                                   link,
                                   ~ gt_hyperlink(.x, .y))) |>
                # truncate description
                mutate(description = stringr::str_trunc(description,
                                                        description_length))
        
}

gt_predictions = function(data) {
        
        data |>
                gt() %>%
                cols_hide(columns = c(published, game_id, link)) %>%
                gt_img_rows(columns = image, height = 100) %>%
                cols_align(
                        columns = c(rank, image, .pred_yes, own),
                        align = "center"
                ) %>%
                cols_align(
                        columns = c("description", "game"),
                        align = "left"
                ) %>%
                data_color(
                        columns = c(".pred_yes"),
                        method = "numeric",
                        na_color = "grey60",
                        autocolor_text = T,
                        palette = c("white", "dodgerblue2"),
                        domain = c(0, 1)
                ) |>
                data_color(
                        columns = c("own"),
                        method = "factor",
                        na_color = "white",
                        autocolor_text = T,
                        fn = function(x) case_when(x == 'yes'  ~ 'dodgerblue2',
                                                   TRUE ~ 'white')
                ) |>
                # set columns
                cols_move(
                        columns = c("rank", "image", "game", "description", ".pred_yes", "own"),
                        after = "rank"
                ) |>
                cols_label(
                        rank = "Rank",
                        image = "Image",
                        description = "Description",
                        game = "Game",
                        own = 'Own',
                        .pred_yes = 'Pr(Own)'
                ) |>
                cols_width(
                        rank ~ px(100),
                        image ~ px(150),
                        game ~ px(150),
                        description ~ px(400)
                ) |>
                fmt_number(
                        columns = starts_with(".pred"),
                        decimals = 3
                ) |>
                gt::opt_interactive(
                        use_filters = T,
                        use_resizers = T
                )
}