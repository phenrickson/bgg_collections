# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(googleCloudStorageR)
library(dplyr)
library(tidyr)
library(purrr)

# load source code
targets::tar_source("src")

# files to upload
files = list.files("docs")
# reports
reports = files[grepl("*.html", files)]

# upload
map(reports, ~ upload_report(file = paste0('docs/', .x)))