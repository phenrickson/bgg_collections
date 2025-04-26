# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(googleCloudStorageR)
library(dplyr)
library(tidyr)
library(purrr)

# set env based on config
args <- commandArgs(trailingOnly = TRUE)
env <- if (length(args) > 0) args[1] else "test-reports"

Sys.setenv(R_CONFIG_ACTIVE = env)
cfg <- config::get(config = env)

# load source code
targets::tar_source("src")

# files to upload
files <- list.files("docs")
# reports
reports <- files[grepl("*.html", files)]

# upload all reports
map(reports, ~ upload_report(prefix = cfg$prefix, 
                             bucket = cfg$bucket, 
                             file = paste0("docs/", .x)))
