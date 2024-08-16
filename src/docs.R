# function to render quarto report and output given username
render_report = function(username,
                         input,
                         metrics,
                         outcome,
                         pin_vetiver,
                         
                         ...) {
        
        file = glue::glue("{username}.html")
        
        quarto::quarto_render(
                input = input,
                execute_params = list(username = username,
                                      outcome = outcome,
                                      metrics = metrics,
                                      pin_vetiver = pin_vetiver),
                output_file = file,
                ...
        )
        
        file
}

# function to upload
upload_report = function(file,
                         prefix = config::get("prefix"),
                         bucket = config::get("bucket"),
                         predefinedAcl = 'bucketLevel',
                         type = 'text/html') {
        
        name = paste0(prefix, file)
        googleCloudStorageR::gcs_upload(file = file,
                                        name = name,
                                        bucket = bucket,
                                        type = type,
                                        predefinedAcl = predefinedAcl)
        
        name
}

# # not run
# upload_report(file = here::here('docs/phenrickson.html'))