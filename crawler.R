#! Cron me to run at midnight

# This script crawls the file scan path specified in the config for files
# with valid meta data and truncserts the jobs table into the statekeeper db.

library(ductwork)
library(dplyr)

files = list_files(config$scan_path,
                   config$file_types)

jobs = list_meta(files) %>%
       mutate(state = "waiting") %>%
       load_jobs(config$statekeeper)

