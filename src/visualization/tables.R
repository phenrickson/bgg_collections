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

make_image_link = function(link,
                           height = 52) {
        
        paste0('<img src =',
               link,
               ' height=',
               paste(height, sep=""),
               '>',
               '</img>')
        
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
                # remove if missing image
                filter(!is.na(image)) |>
                # make link
                mutate(game = map2(paste0(game, " (", published, ")"),
                                   link,
                                   ~ gt_hyperlink(.x, .y))) |>
                # truncate description
                mutate(description = stringr::str_trunc(description,
                                                        description_length)) |>
                arrange(desc(.pred_yes))
        
}

gt_styling = function(tab) {
        
        tab |>
                tab_options(heading.align = "left",
                            column_labels.border.top.style = "none", 
                            table.border.top.style = "none", 
                            column_labels.border.bottom.style = "none", 
                            column_labels.border.bottom.width = 1,
                            column_labels.border.bottom.color = "#334422", 
                            table_body.border.top.style = "none", 
                            table_body.border.bottom.color = "white", 
                            heading.border.bottom.style = "none",
                            data_row.padding = px(7), 
                            column_labels.font.size = px(12))
}

gt_predictions_styling = function(tab,
                                  ...) {
        
        tab |>
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
                gt::fmt_number(
                        columns = starts_with(".pred"),
                        decimals = 3
                )
}

gt_predictions = function(data,
                          ...) {
        
        data |>
                gt() |>
                gt_predictions_styling() |>
                gt_styling() |>
                gt::opt_interactive(
                        use_filters = T,
                        use_resizers = T,
                        ...
                )
}



make_collection_datable = function(collection_table,
                                   page_length = 10) {
        
        
        rating = seq(3, 13)
        complexity = seq(0.5, 5.5, by = 0.1)
        
        color = 'dodgerblue2'
        low_color = 'deepskyblue1'
        high_color = 'orange'
        
        my_color_ramp = colorRampPalette(c("white", color))
        
        complexity_color_ramp = colorRampPalette(c(low_color, "white", high_color))
        
        max_color = my_color_ramp(length(rating)-3)[length(rating)-3]
        
        collection_table %>%
                mutate(Published = as.integer(published),
                       Image = make_image_link(image),
                       # Image = paste0('<img src =',
                       #                image,
                       #                ' height="52"></img>'),
                       Game = game,
                       Best = playercount_best,
                       Recommended = playercount_rec,
                       #    Time = `playing time`,
                       Complexity = round(averageweight, 2),
                       Rating = rating,
                       .keep = 'none') %>%
                DT::datatable(escape=F,
                              rownames = F,
                              extensions = c('Responsive'),
                              #  caption = "Games",
                              class = list(stripe =F),
                              filter = list(position = 'top'),
                              options = list(pageLength = page_length,
                                             initComplete = htmlwidgets::JS(
                                                     "function(settings, json) {",
                                                     paste0("$(this.api().table().container()).css({'font-size': '", '10pt', "'});"),
                                                     "}"),
                                             scrollX=F,
                                             columnDefs = list(
                                                     list(className = 'dt-center',
                                                          visible=T,
                                                          targets=c("Image",
                                                                    "Published",
                                                                    "Best",
                                                                    "Recommended",
                                                                    "Complexity",
                                                                    "Rating")
                                                     )
                                             )
                              )
                ) %>%
                DT::formatStyle(
                        columns = "Rating",
                        backgroundColor = 
                                DT::styleInterval(
                                        cuts = rating,
                                        values = my_color_ramp(length(rating)+1)
                                )
                ) %>%
                DT::formatStyle(
                        columns = "Complexity",
                        backgroundColor = 
                                DT::styleInterval(
                                        cuts = complexity,
                                        values = complexity_color_ramp(length(complexity)+1)
                                )
                )
}

prep_collection_datatable = function(collection,
                                     games) {
        
        
        playercounts = 
                games_raw |>
                inner_join(
                        collection |>
                                select(game_id),
                        by = join_by(game_id)
                ) |>
                bggUtils:::unnest_playercounts() |>
                bggUtils:::wider_playercounts()
        
        outcomes = 
                games_raw |>
                bggUtils:::unnest_outcomes()
        
        info =
                games_raw |>
                inner_join(
                        collection |>
                                select(game_id),
                        by = join_by(game_id)
                ) |>
                bggUtils:::unnest_info()
        
        collection |>
                left_join(
                        info |>
                                select(-name),
                        by = join_by(game_id)
                ) |>
                left_join(
                        playercounts,
                        by = join_by(game_id)
                ) |>
                left_join(
                        outcomes,
                        by = join_by(game_id)
                ) |>
                mutate(
                        playingtime = case_when(
                                playingtime < 15 ~ '<15',
                                playingtime < 30 ~ '<30',
                                playingtime >=30 & playingtime <=60 ~ '30-60',
                                playingtime > 60 & playingtime <=120 ~ '61-120',
                                playingtime > 121 & playingtime <=180 ~ '121-180',
                                playingtime > 180 ~ '180+',
                                TRUE ~ as.character(playingtime)
                        )
                ) |>
                mutate(
                        id = game_id,
                        game = name,
                        published = yearpublished,
                        best = playercount_best,
                        recommnded = playercount_rec,
                        complexity = averageweight,
                        rating = rating
                )
        
}