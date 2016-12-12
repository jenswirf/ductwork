

#' Read jobs currently in statekeeper
#' @export
read_jobs = function(statekeeper) {

  r = NULL
  attempt = 1
  while(is.null(r) && attempt <= 5) {
    try(silent = T, expr =
    {
      db = dplyr::src_sqlite(statekeeper)
      jobs = dplyr::tbl_df(DBI::dbGetQuery(db$con, "select * from jobs"))
      r = TRUE
    })
    attempt = attempt + 1
    if (is.null(r)) Sys.sleep(10)
  }
jobs
}

#' Check if job is due to run
#' @export
is_due = function(schedule) {

  t = c("minute", "hour", "dayofmonth", "monthofyear", "dayofweek")

  # parse input
  schedule = stringr::str_split(stringr::str_trim(schedule, "both"), " ")

  # cron format current time and date
  current = stringr::str_split(format(lubridate::now(tzone = config$tz), '%M %H %d %m %u'), " ")[[1]] %>% as.list
  names(current) = t

  due = function(schedule, current) {

    if (length(schedule) != 5) return(FALSE)
    if (is.null(schedule) | is.na(schedule)) return(FALSE)

    schedule = as.list(schedule)
    names(schedule) = t

    all(schedule$minute == "*" | int(schedule$minute) == int(current$minute),
        schedule$hour == "*" | int(schedule$hour) == int(current$hour),
        schedule$dayofmonth == "*" | schedule$dayofmonth == current$dayofmonth,
        schedule$monthofyear == "*" | schedule$monthofyear == current$monthofyear,
        schedule$dayofweek == "*" | schedule$dayofweek == current$dayofweek
    )

  }

  suppressWarnings(sapply(schedule, due, current))

}

#' Check if a jobs dependencies are satisfied
#' @export
is_dependecy_satisfied = function(depends) {

  satisfied = function(d) {

    dependencies = d %>% stringr::str_split(",") %>% unlist %>% stringr::str_trim(side = "both")

    out = character()
    for (dependency in dependencies) {
      dependency_status = (jobs %>% filter(id == dependency))$state
      if (!length(dependency_status))
        dependency_status = FALSE
      out = c(out, dependency_status == "done")
    }
    all(out)
  }

  suppressWarnings(sapply(depends, satisfied))

}


#' Manaully trigger a job
#' @export
manual_run_job = function(job_id, verbose = TRUE) {

  if (missing(job_id))
    stop("Needs a job_id")

  l(sprintf("Yippee ki yay %s", job_id))

  jobs = read_jobs(config$statekeeper)

  job = jobs %>%
    filter(id == job_id)

  if (nrow(job) == 0)
    stop("No such job.")

  run(job, update = T, notification = F, logging = T, verbose = verbose)

}



#' Excecute a job
#' @export
run = function(jobs, wid = make_worker_id(lubridate::now), logging = T, update = T, notification = T, verbose = F) {

  results = list()

  m = sprintf("WID: %s is taking: %s", wid, jobs$id %>% unlist %>% paste0(collapse = ", ")); l(m)
  if (verbose) message(m)

  for (i in 1:nrow(jobs)) {

    job = jobs[i,]

    m = sprintf("Working on %s", job$id); l(m)
    if (verbose) message(m)

    if (update)
      update_state(job, "running")

    runner = switch(job$type,
                    R   = run.R,
                    SQL = run.SQL,
                    PY  = run.PY,
                    SH  = run.SH)

    lines = parse_file(job$file)$body
    result = timer(runner, lines)

    m = sprintf("Job %s: %s in %s seconds",
                ifelse(result$success, "succeded", "failed"),
                job$id,
                round(result$duration)); l(m)

    l("Output:");l(result$output)
    l("Error:");l(result$error)
    if (verbose) {
      message("Output:");message(result$output);message(result$error)
    }

    if (update)
      update_state(job, ifelse(result$success, "done", "failed"))

    if (logging)
      log_run(job, result, wid)

    if (notification)
      notify(job, result)

    results[[job$id]] = result %>% unlist

    if (verbose) message(m)

  }
  results
}

#' Send notification of failed job to slack
#' @export
notify = function(job, result) {

  if (result$success)
    return(NULL)

  statements = c("Well :shit:!",
                 "Oh noes! :scream:",
                 "Ffs :facepalm:",
                 "Oh my god! :face_with_rolling_eyes:",
                 "Crap! :shit:",
                 "No :cake: for you!",
                 "I'm sorry! :pensive:",
                 "Gah! :confounded:",
                 "Did you need this? :troll:",
                 "Problem? :troll:",
                 ":computer: says no!",
                 "I've fallen and I can't get up! :nerd_face:",
                 "Please. God. No. :pray::cloud:",
                 "Not again! :angry:",
                 "60% of the time, it works every time! :v:",
                 "Bad news! :newspaper:",
                 "You are not going to like this... :neutral_face:",
                 "Heads up! :warning:",
                 "Out of bounds! :golf:",
                 "Man over board! :speedboat::scream:",
                 "I'm in a glass cage of emotion :confounded:",
                 "Who you gonna call? :ghost:",
                 "Home. Phone home! :alien",
                 "Where did you get that code - the toilet store? :toilet:",
                 "Man down! :face_with_head_bandage:",
                 "Alarm! :speaking_head_in_silhouette:",
                 "Cannot compute :robot_face:",
                 "Houston: Oh oh! :rocket:",
                 "THIS IS SPARTA!! :rage1:",
                 "Um, how bad do you need this data? :confused:",
                 "Oh snap! :zap:",
                 "yer a wizard, 'arry :wizard:",
                 "Keep the change, you filthy animal. :pig:",
                 "I'll be back. :muscle:",
                 "Trouble in paradise! :broken_heart:",
                 "Sad :panda_face:!",
                 "Meeedic! :helmet_with_white_cross:",
                 "Tough luck! :slightly_frowning_face:",
                 "Bummer! :disappointed:",
                 "I'm to :older_man::skin-tone-3: for this!",
                 ":grimacing::gun:",
                 "We need to go deeper! :cloud:",
                 "Now, say my name. :eyeglasses:",
                 "Y u no work? :yuno:",
                 "Wtf?? :wtf:",
                 "Yeeeah, if you could get this done today, that'd be great. :necktie::coffee:",
                 "You were the chosen one. :pill:",
                 "Yo :dog:!",
                 "Mayday, mayday! :airplane:",
                 "We're gonna need a bigger :motor_boat:",
                 "Game over man, game over! :controller:",
                 "Tiny :violin:",
                 "Git pull and prosper :spock-hand:",
                 "Let us all sing: 'we shall overcome' :earth_americas:",
                 "Road work ahead :construction:",
                 "Going up shit creek :rowboat:",
                 "R.I.P :murica::funeral_urn:",
                 ":see_no_evil::hear_no_evil:",
                 "You've got :mailbox_with_mail:",
                 "OVER THE LINE! :bowling:",
                 "Wiiiiiiiiiiiiilson!!! :volleyball:",
                 "NO SOUP FOR YOU! :stew:",
                 "I wonder what happened!? :sunglasses:",
                 "Hammer time! :hammer:",
                 "Why so serious? :black_joker:",
                 "You have work to do! :wrench:",
                 "Need a hug? :hugging_face:",
                 "This one needs more :duct_tape:!",
                 "It's fail o':clock1:!",
                 "There is a leak in the pipe! :surfer: ",
                 "Fire! :fire::fire::fire:"
  )



  msg = sprintf("Ping @%s %s The job *_%s_* failed at %s..", job$author, sample(statements, 1), job$id, format(lubridate::now(), "%H:%M"))

  try({
    slack_push(msg,
               channel = config$slack$channel,
               username = config$slack$user,
               icon_emoji = config$slack$icon,
               incoming_webhook_url = config$slack$webhook)
  }, silent = T)

}


#' Update the state of a job in statekeeper
#' @export
update_state = function(x, new_state) {

  db = dplyr::src_sqlite(config$statekeeper)

  for (id in x$id) {

    r = NULL
    attempt = 1
    while(is.null(r) && attempt <= 5) {
      try(silent = T, expr =
      {
        DBI::dbSendQuery(db$con, sql(sprintf("update jobs set state = '%s' where id = '%s'", new_state, id)))
        r = TRUE
      })
      attempt = attempt + 1
      if (is.null(r)) Sys.sleep(5)
    }

  }

  dplyr::mutate(x, state = new_state)

}








# internals ---------------------------------------------------------------

run.R = function(lines) {

  result = tryCatch({
    tmp = file.path(config$local_tmp_dir, paste0("tmp_", as.character(as.integer(runif(1)*1e+5)), ".R"))
    write(lines, file = tmp)
    system("export LANG=en_US.UTF-8;export LC_ALL=en_US.UTF-8")
    out = command(sprintf("unset R_HOME; %s %s ", config$r, tmp))
    #unlink(tmp)
    result = list(success = ifelse(out$status == 0, T, F),
                  output = out$output,
                  error = out$error)
    result
  },
  error = function(e) {
    if (!exists("output")) output = NA
    return(list(success = FALSE, error = e))
  })

  result
}




run.SQL = function(lines, db = config$sql[1]) {

  db = db %>% unname %>% .[[1]]

  cmd = switch(db$type,
               mysql = execute.mysql,
               postgres = execute.postgres)


  tmp = file.path(config$local_tmp_dir, paste0("tmp_", as.character(as.integer(runif(1)*1e+5)), ".sql"))
  write(lines, file = tmp)

  out = cmd(host = db$host,
            user = db$username,
            password = db$password,
            database = db$database,
            port = db$port,
            file = tmp)

  result = list(success = ifelse(out$status == 0, T, F),
                output = out$output,
                error = out$error)

  result

}


execute.postgres = function(host, user, password, database, port, file) {
  "PGPASSWORD='%s' %s -h %s -p %s -U %s -d %s -f %s -v ON_ERROR_STOP=1" %>%
    sprintf(password, config$psql, host, port, user, database, file) %>%
    command
}

execute.mysql = function(host, user, password, database, port, file) {
  "%s -h %s -P %s -u %s --password='%s' -c %s < %s" %>%
    sprintf(config$mysql, host, port, user, password, database, file)
}


run.PY = function(lines) {

  result = tryCatch({
    tmp = file.path(config$local_tmp_dir, "tmp.py")
    write(lines, file = tmp)
    out = command(stringr::str_c("python ", tmp))
    unlink(tmp)
    result = list(success = ifelse(out$status == 0, T, F),
                  output = out$output,
                  error = out$error)
    result
  },
  error = function(e) {
    if (!exists("output")) output = NA
    return(list(success = FALSE, error = e))
  })

  result
}




run.SH = function(lines) {

  result = tryCatch({
    tmp = file.path(config$local_tmp_dir, "tmp.sh")
    write(lines, file = tmp)
    out = command(stringr::str_c("sh ", tmp))
    unlink(tmp)
    result = list(success = ifelse(out$status == 0, T, F),
                  output = out$output,
                  error = out$error)
    result
  },
  error = function(e) {
    if (!exists("output")) output = NA
    return(list(success = FALSE, error = e))
  })

  result

}
