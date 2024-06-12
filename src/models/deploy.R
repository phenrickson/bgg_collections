vetiver_user_model = function(model_obj) {
        
        
        wflow_id = model_obj |> pluck("wflow_id")
        user = model_obj |> pluck("username")
        settings = model_obj |> pluck("settings", 1)
        
        wflow = model_obj |> pluck("wflow", 1)
        mold = wflow |> extract_mold()
        ids = mold$extras$roles$id |> select(game_id)
        split = model_obj |> pluck("split", 1) |> bind_rows()
        data = ids |> inner_join(split, by = join_by("game_id"))
        
        model_name = user
    #    
        wflow |>
                vetiver::vetiver_model(model_name = model_name,
                                       metadata = list("settings" = settings,
                                                       "data" = data,
                                                       "username" = user))
        
}

pin_model = function(vetiver_model, board, ...) {
        
        vetiver::vetiver_pin_write(board = board,
                                   vetiver_model = vetiver_model,
                                   ...)
        
        meta = 
                pins::pin_meta(board = board, vetiver_model$model_name)
        
        meta$pin_hash
}

load_model = function(board, name, version, hash = NULL, ...) {
        
        if (!is.null(hash)) {
                message(hash)
        }
        
        vetiver::vetiver_pin_read(board = board,
                                  name = name,
                                  ...)
}