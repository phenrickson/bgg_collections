upload_report = function(file,
                         bucket = 'bgg_reports',
                         predefinedAcl = 'bucketlevel',
                         type = 'text/html') {
        
        split = strsplit(file, '/projects') |> 
                unlist()
        
        name = split[2]
        
        print(name)
}

# # not run
# upload_report(file = here::here('docs/phenrickson.html'))