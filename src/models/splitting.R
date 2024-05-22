# function to split collection based on yearpublished
split_by_year = function(data,
                         end_train_year) {
        
        train = 
                data |>
                filter(yearpublished <= end_train_year)
        
        test = 
                data |>
                filter(yearpublished > end_train_year)
        
        make_splits(
                list(analysis =
                             seq(nrow(train)),
                     assessment =
                             nrow(train) + seq(nrow(test))),
                bind_rows(train, test)
        )
}

extract_split = function(model) {
        
        model |>
                pluck("split", 1) |>
                bind_rows(.id = 'type') |>
                group_by(username, type) |>
                mutate(years = paste(min(yearpublished), max(yearpublished), sep = "-")) |>
                ungroup()
}