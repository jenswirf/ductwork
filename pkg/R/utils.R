#' Read ductwork config file
#' @export
read_config = function(path) {

  if (missing(path)) {
    cat("Config file path not specified. Trying /etc/ductwork.config")
    path = "/etc/ductwork.config"
  }

  tryCatch({
    config = yaml::yaml.load_file(path)
  },
  error = function(e) {
    stop("Config file could not be read.", e)
  })

  config
}



# internals ---------------------------------------------------------------

is_blank <- function (x)
{
  if (length(x))
    all(grepl("^\\s*$", x))
  else TRUE
}

trim_trailing_ws <- function (x) {
  sub("\\s+$", "", x)
}

str_trim_whitespace <- function(x) return(gsub("^ *|(?<= ) | *$", "", x, perl=T))

# parse_sql_queries = function(lines) {
#
#   queries = lines %>% stringr::str_c(collapse = " \r ") %>% stringr::str_split(pattern = ";") %>% unlist %>% str_trim_whitespace
#   queries = queries[queries != ""]
#   queries = stringr::str_c(queries, ";")
#   queries
# }


slack_push = function (msg, channel, username, icon_emoji, incoming_webhook_url)
{
  data <- as.character(msg)
  icon_emoji <- sprintf(", \"icon_emoji\": \"%s\"", icon_emoji)
  output <- gsub("^\"|\"$", "", jsonlite::toJSON(data, simplifyVector = TRUE, flatten = TRUE, auto_unbox = TRUE))
  resp <- httr::POST(url = incoming_webhook_url, httr::add_headers(`Content-Type` = "application/x-www-form-urlencoded",
               Accept = "*/*"), body = utils::URLencode(sprintf("payload={\"channel\": \"%s\", \"username\": \"%s\", \"text\": \"%s\"%s}",
               channel, username, output, icon_emoji)))
  warn_for_status(resp)

  return(invisible())
}

int = as.integer
char = as.character
str_lower = tolower
str_upper = toupper
stopif = function(x) if (any(x)) quit(save = "no", status = 0)
empty = function(x) ifelse(nrow(x) > 0, F, T)

make_worker_id = function(x) {
  stringr::str_c(str_upper(format(x(), '%a')), format(x(), "%Y%m%d-%H-%M-%S-P"), Sys.getpid())
}

sysinfo = function() {
  i = Sys.info() %>% as.list
  sprintf("%s @ %s", i$user, i$nodename)
}

parse_print = function(x) {

  out = tryCatch({
    y = x[!stringr::str_detect(x, "LC_|During|Attaching|masked|package:|Auto-disconnecting|<80>|<98>")]
    out = stringr::str_replace_all(y, '\\"|\\[1\\]', " ") %>% stringr::str_trim(side = "both") %>% stringr::str_c(collapse = " ")
  }, error = function(e) {
    return("")
  })

  if (length(out) == 0) out = NA
  out
}

timer = function(FUN, ...) {

  start = lubridate::now()
  out = FUN(...)
  end = lubridate::now()

  append(out, list(start = start,
                   end = end,
                   duration = signif(as.numeric(end-start,units="secs"), 3)))
}




command = function(cmd) {

  tmp_stder = file.path(config$local_tmp_dir, "stderr.tmp")
  tmp_stdout =  file.path(config$local_tmp_dir, "stdout.tmp")

  out = list()
  out$status = system(stringr::str_c(cmd, " 2> ", shQuote(tmp_stder), " > ", shQuote(tmp_stdout)))
  out$error = readLines(tmp_stder)
  out$output = readLines(tmp_stdout)

  #unlink(c(tmp_stder, tmp_stdout))

  out
}


#' @importFrom magrittr %>%
#' @export
magrittr::"%>%"

`%||%` <- function(x, y) if(is.null(x) | length(x) == 0) y else x
