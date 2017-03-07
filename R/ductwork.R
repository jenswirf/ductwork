

init = function() {

  Sys.setlocale('LC_ALL','C')

  # Load config file
  config <<- read_config("ductwork/ductwork.config")

  # Initiate statekeeper
  try({init_statekeeper(config$statekeeper)})

  # Initiate logger
  try({init_logger(config$logger)})

  message("Ductwork is loaded.")

}


# FUTURE UPGRADES
# ---------------
# manul run job
# retry option
# crawl and update only
# dependency on a group
# truncate ouput/errors
