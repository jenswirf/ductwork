#! Cron me to run continuously every minute

# Checks db if there is something to run, either a job scheduled for now or if
# a job that that has a dependency that is completed.
# In case there is more than 1 eligible job then run them in succession.

library(ductwork)
library(dplyr)

jobs = read_jobs(config$statekeeper)

due_jobs = jobs %>%
  filter(state == "waiting") %>%
  filter(is_due(schedule) | is_dependecy_satisfied(depends))

stopif(empty(due_jobs))

due_jobs %>%
  update_state("queued") %>%
  run(.)

