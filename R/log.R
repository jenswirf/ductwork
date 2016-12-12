
#' Create log database if doesn't exist
#' @export
init_logger =  function(logger) {

  try({log = dplyr::src_sqlite(logger, create = T)})

  if (!file.exists(logger)) {
    try({
      entries = c("wid" = "character",
                  "id" = "character",
                  "type" = "character",
                  "start" = "character",
                  "end" = "character",
                  "duration" = "character",
                  "status" = "character",
                  "error" = "character",
                  "output" = "character",
                  "info" = "character")

      dplyr::db_create_table(log$con, table = "log", types = entries)
    })
  }

  # delete old log entries so it doesn't fill up
  try({
    cutoff = lubridate::today() - lubridate::days(6)
    DBI::dbSendQuery(log$con, sprintf("delete from log where start < '%s'", cutoff))
    DBI::dbDisconnect(log$con)
  })


  message("Logger initiated")
  NULL
  invisible()
}


#' Create statekeeper if doesn't exist
#' @export
init_statekeeper = function(statekeeper) {

  if (!file.exists(statekeeper))
    try(dplyr::src_sqlite(statekeeper, create = T))

  message("Statekeeper initiated")
}



#' Read a file and partition it into meta/body
#' @export
read_log = function(logger) {

  log = dplyr::src_sqlite(logger)
  lines = dplyr::tbl(log, "log") %>% collect
  DBI::dbDisconnect(log$con)

  lines
}

#' Add an entry to log
#' @export
log_run = function(job, result, wid) {

  try({

  logger = dplyr::src_sqlite(config$logger)

  entry = data_frame(wid = wid,
                     id = job$id,
                     type = job$type,
                     start = char(result$start),
                     end = char(result$end),
                     duration = result$duration,
                     status = ifelse(result$success, "success", "fail"),
                     error = parse_print(result$error),
                     output = parse_print(result$output),
                     info = sysinfo())

  logger %>%
    add_log_entry(entry)
  })

  try({
    DBI::dbDisconnect(logger$con)
    })
}

# internals ---------------------------------------------------------------


add_log_entry = function(log, x) {

  try({
    columns = stringr::str_c(names(x), collapse = ",")
    values = as.character(x) %>% stringr::str_replace_all("'",'"')  %>%stringr::str_c(collapse = "','")
    DBI::dbSendQuery(log$con, sprintf("insert into log (%s) values ('%s')", columns, values))
    DBI::dbDisconnect(log$con)
    })

    NULL
    invisible()
  }


l = function(x) {

  line = sprintf("[%s] %s", lubridate::now(), x)
  try({
    write(line,file=config$worklog,append=TRUE)
  })


}
