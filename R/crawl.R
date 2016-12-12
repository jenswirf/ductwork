
#' List files of specified type in path
#' @export
list_files = function(path, file_types) {
  l("Listing files.")
  files = list.files(path, pattern = stringr::str_c("\\.(", stringr::str_c(file_types, collapse = "|"), ")$"), recursive = T, ignore.case = T)
  dplyr::data_frame(file = stringr::str_c(path, files))
}


#' Read a file and partition it into meta/body
#' @export
parse_file = function(path, debug = F) {
  
  meta = try({
    input_lines = readLines(file.path(path), warn = FALSE)
    partitioned = partition_yaml_front_matter(input_lines)
    parse_yaml_front_matter(partitioned$front_matter)
    }, silent = T)

  if (!is_valid_meta(meta) && !debug) return(NULL)

  list(meta = meta,
       body = partitioned$body)
}


#' Parse meta data and list valid jobs
#' @export
list_meta = function(files) {
  l("Listing jobs.")
  out = data.frame()

  for (file in files$file) {

    meta = parse_file(file)$meta

    tags = jsonlite::toJSON(meta[!(names(meta) %in% c("id", "author", "schedule", "depends"))])
    tags = ifelse(tags == "{}", "", tags)

    if (!is.null(meta)) {
      meta = dplyr::data_frame(id = meta$id,
                        type = str_upper(tools::file_ext(file)),
                        author = meta$author %||% NA,
                        schedule = meta$schedule %||% NA,
                        depends = meta$depends %||% NA,
                        grp = meta$group %||% NA,
                        file = file,
                        tags = tags
      )
    }
    out = dplyr::bind_rows(out, meta)
  }

  out
}

#' Load jobs into statekeeper
#' @export
load_jobs = function(x, statekeeper) {
  l("Starting loading jobs.")

  db = dplyr::src_sqlite(statekeeper)

  r = NULL
  attempt = 1
  while(is.null(r) && attempt <= 5) {
    try(silent = T, expr =
      {
        DBI::dbSendQuery(db$con, "drop table jobs")
        DBI::dbSendQuery(db$con, sprintf("create table jobs (%s)", stringr::str_c(names(x), " TEXT", collapse = ",")))
        insert_into(db$con, "jobs", x)
        r = TRUE
      })
  attempt = attempt + 1
  if (is.null(r)) Sys.sleep(15)
  }

  l("Done loading jobs.")
  read_jobs(statekeeper)

}



insert_into = function(con, name, values) {
  cols <- lapply(values, escape, collapse = NULL, parens = FALSE) #, con = con)
  col_mat <- matrix(unlist(cols, use.names = FALSE), nrow = nrow(values))

  rows <- apply(col_mat, 1, paste0, collapse = ", ")
  values <- paste0("(", rows, ")", collapse = "\n, ")

  statement = dplyr::build_sql(sql(sprintf("INSERT INTO %s VALUES ", name)), sql(values))
  DBI::dbSendQuery(con, statement)
}

manual_crawl = function(trunc = F) {


}

#' Test if job has valid meta data and optionally to test run the code.
#' @export
test_job = function(path) {

  content = parse_file(path, debug = T)

  if (!is_valid_meta(content$meta)) {
    message("Meta data is not valid:")
    is_valid_meta(content, debug = T)
  } else {
    message("Meta data is OK!")
  }

  answer = readline("Continue and try and run code? (y/n): ")
  if (str_lower(answer) == "y") {
    manual_run_job(path = path)
  }

}
# internals ---------------------------------------------------------------




is_valid_meta = function(meta, debug = F) {

  err = NULL

  if (length(meta) == 0) {
    err = "Meta has 0 elements."
    if (!debug) return(FALSE)
      }
  if (!"schedule" %in% names(meta) & !"depends" %in% names(meta)) {
    err = "Neither 'schedule' or 'depends' present."
    if (!debug) return(FALSE)
  }
  if (!"id" %in% names(meta)) {
    err = "'id' not present"
    if (!debug) return(FALSE)
  }

  if (!debug) return(TRUE)

  if (is.null(err)) {
    return("Meta data OK")
  } else {
    return(sprintf("ERROR: %s", err))
  }

}

parse_yaml_front_matter <- function(input_lines) {

  partitions <- partition_yaml_front_matter(input_lines)
  if (!is.null(partitions$front_matter)) {
    front_matter <- partitions$front_matter
    if (length(front_matter) > 2) {
      front_matter <- front_matter[2:(length(front_matter)-1)]
      front_matter <- paste(front_matter, collapse="\n")
      validate_front_matter(front_matter)
      parsed_yaml <- yaml::yaml.load(front_matter)
      if (is.list(parsed_yaml))
        parsed_yaml
      else
        list()
    }
    else
      list()
  }
  else
    list()
}

validate_front_matter <- function(front_matter) {
  front_matter <- trim_trailing_ws(front_matter)
  if (grepl(":$", front_matter))
    stop("Invalid YAML front matter (ends with ':')", call. = FALSE)
}

partition_yaml_front_matter <- function(input_lines) {

  validate_front_matter <- function(delimiters) {
    if (length(delimiters) >= 2 && (delimiters[2] - delimiters[1] > 1)) {
      # verify that it's truly front matter (not preceded by other content)
      if (delimiters[1] == 1)
        TRUE
      else
        is_blank(input_lines[1:delimiters[1]-1])
    } else {
      FALSE
    }
  }

  # is there yaml front matter?
  delimiters <- grep("^---\\s*$", input_lines)
  if (validate_front_matter(delimiters)) {

    front_matter <- input_lines[(delimiters[1]):(delimiters[2])]

    input_body <- c()

    if (delimiters[1] > 1)
      input_body <- c(input_body,
                      input_lines[1:delimiters[1]-1])

    if (delimiters[2] < length(input_lines))
      input_body <- c(input_body,
                      input_lines[-(1:delimiters[2])])

    list(front_matter = front_matter,
         body = input_body)
  }
  else {
    list(front_matter = NULL,
         body = input_lines)
  }
}
