library(ductwork)
library(dplyr)
library(lubridate)
library(stringr)
library(formattable)
library(htmlwidgets)
library(shinyjs)
library(shinydashboard)
library(tidyr)
library(ggplot2)
library(scales)
library(directlabels)


state_table = function(input) {

  jobs = read_jobs(config$statekeeper)

  last_run = read_log(config$logger) %>%
    arrange(desc(ymd_hms(start))) %>%
    group_by(id) %>%
    summarise(last_run = first(start))


  out = jobs %>%
    left_join(last_run) %>%
    mutate(logic = ifelse(is.na(schedule), depends, schedule)) %>%
    mutate(`  ` = ifelse(is.na(schedule), F, T)) %>%
    mutate(group = ifelse(is.na(grp), "", grp)) %>%
    mutate(`last run` = "") %>%
    mutate(`last run` = pretty_time(ymd_hms(last_run))) %>%
    mutate(` ` = state) %>%
    mutate(github = sprintf("<a target='_blank' href='%s' class='github-link'><i class=\"glyphicon glyphicon-link\"></i></a>", github_link(file))) %>%
  #  mutate(rerun = "<button id=\"rerun\" type=\"button\" class=\"btn btn-default action-button shiny-bound-input\"><i class=\"glyphicon glyphicon-refresh\"></i></button>") %>%
    mutate(rerun = sprintf("<a onclick=\"write_cmd('%s')\" class=\"link-icon\" href=\"#shiny-tab-manual\" data-toggle=\"tab\" data-value=\"manual\"><i class=\"glyphicon glyphicon-refresh\"></i></a>", id)) %>%
    select(id, type, author, `  `, logic, ` `, state, group, `last run`, github, rerun)

    # filtering
    out.filtered = out

    try({

      if ((input$type != "Any") & length(input$type) != 0)
        out.filtered = out.filtered %>% filter(type == input$type)

      if (input$state != "Any")
        out.filtered = out.filtered %>% filter(state == tolower(input$state))

      if (input$author != "Anyone")
        out.filtered = out.filtered %>% filter(author == tolower(input$author))

      if (input$logic != "Any")
        out.filtered = out.filtered %>% filter(`  ` == input$logic)

      if (input$group != "Any")
        out.filtered = out.filtered %>% filter(group == tolower(input$group))

    }, silent = T)



    out.table = out.filtered %>% formattable(list(
      state = state_format(),
      logic = formatter("span", style = x ~ style(display = "block", `text-align` = "left")),
      type = formatter("span", style = x ~ style(display = "block", `text-align` = "center", `font-weight` = "bold", padding = "1px 2px", `border-radius` = "3px", `background-color` = "#F2F2F2")),
      id = formatter("span", style = x ~ style(display = "block", `text-align` = "left")),
      ` ` = formatter("span", style = function(x) style(display = "block", `text-align` = "left", padding = "1px 4px", `border-radius` = "3px", color = map_state_color(x)),
                      x ~ icontext(map_state_icon(x), "")),
      `  ` = formatter("span", style = function(x) style(display = "block", `text-align` = "left", padding = "1px 0px 1px 18px", `border-radius` = "3px", color = "#282828"),
                       x ~ icontext(ifelse(x, "time", "forward"), ""))

    ))




  out.table
}

# summary_stats = function() {
#
#   jobs = read_jobs(config$statekeeper)
#
#   jobs %>%
#     count(state) %>%
#     mutate(dummy = 1) %>%
#     spread(state, n) %>%
#     select(-dummy) %>%
#     formattable(list(
#       failed = formatter("span", style = function(x) style(`text-align` = "center", display = "inline-block", padding = "5px 10px", `border-radius` = "3px", `background-color` = "#d9534f", color = "white")),
#       waiting = formatter("span", style = function(x) style(`text-align` = "center", display = "inline-block", padding = "5px 10px", `border-radius` = "3px", `background-color` = "#AEAEAE", color = "white")),
#       done = formatter("span", style = function(x) style(`text-align` = "center", display = "inline-block", padding = "5px 10px", `border-radius` = "3px", `background-color` = "#5cb85c", color = "white")),
#       queued = formatter("span", style = function(x) style(`text-align` = "center", display = "inline-block", padding = "5px 10px", `border-radius` = "3px", `background-color` = "#f0ad4e", color = "white")),
#       running = formatter("span", style = function(x) style(`text-align` = "center", display = "inline-block", padding = "5px 10px", `border-radius` = "3px", `background-color` = "#5bc0de", color = "white"))
#     ))
# }

github_link = function(paths) {


  out = character()
  for (p in 1:length(paths)) {

    path = paths[p]
    rel_path = str_split(path, "/etl/") %>% unlist

    #TODO: Need to reformat this link
    link = sprintf("https://github.com/data/blob/master/etl/%s", rel_path[2])

    if (is.null(link))
      link = ""

    out = c(out, link)

  }
  out
}



log_table = function() {

  log = read_log(config$logger) %>%
    arrange(desc(ymd_hms(start))) %>%
    head(100)

  log %>%
    mutate(duration = str_lower(seconds_to_period(as.integer(duration)))) %>%
    mutate(start = pretty_time(ymd_hms(start)),
           end = pretty_time(ymd_hms(end)),
           error = ifelse(error == "NA", NA, error),
           output = ifelse(output == "NA", "", output)) %>%
    formattable(list(
      status = formatter("span", style = function(x) style(display = "block", `text-align` = "center", padding = "1px 4px", `border-radius` = "3px", `background-color` = ifelse(x == "fail", "#d9534f", "#5cb85c"))),
      `run logic` = formatter("span", style = x ~ style(display = "block", `text-align` = "left")),
      type = formatter("span", style = x ~ style(display = "block", `text-align` = "center", `font-weight` = "bold", padding = "1px 2px", `border-radius` = "3px", `background-color` = "#F2F2F2")),
      id = formatter("span", style = x ~ style(display = "block", `text-align` = "left")),
      error = formatter("span", style = x ~ style(`font-style` = "normal", `background-color` = "rgba(0, 0, 0, 0.04)", `border-radius`= "3px", padding = "0.2em", color = "rgb(51, 51, 51)",
                                                  `font-family` = "Consolas", `font-size` =  "12px"))
      # ,output = formatter("span", style = x ~ style(`font-style` = "normal", `background-color` = "rgba(0, 0, 0, 0.04)", `border-radius`= "3px", padding = "0.2em", color = "rgb(51, 51, 51)",
      #                                             `font-family` = "Consolas", `font-size` =  "12px"))
    ))

}





stats_timeline = function() {

  tl = read_log(config$logger) %>%
    mutate(start = ymd_hms(start),
           end = ymd_hms(end)) %>%
    filter(start >= ymd_hms(str_c(today(), " 00:00:00"))) %>%
    select(id, start, end, status)

  dusk = ymd_hms(str_c(today(), " 00:00:00"))
  dawn = ymd_hms(str_c(today(), " 00:00:00")) + days(1)

  ggplot(tl, aes(id, start, color = status)) +
    geom_linerange(aes(ymin = start, ymax = end + seconds(1), height = 0.2), size = 2.5) +
    scale_color_manual(values = c("#5cb85c", "#d9534f")) +
    theme(legend.position = "none",
          axis.ticks = element_blank(), axis.text.y = element_blank(),
          panel.grid = element_line(colour = NULL),
          panel.grid.major = element_line(colour = "#DEDEDE", linetype = "dotted"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          line = element_line(),
          rect = element_rect(fill = "#FFFFFF", linetype = 0, colour = NA),
          text = element_text(colour = "grey20"),
          panel.background = element_blank(),
          strip.background = element_rect()) +
    coord_flip() + xlab("") + ylab("") +
    geom_dl(aes(label = id), list(cex = 1, vjust = -1.5))


}








pretty_time = function(dts) {

  out = character()

  for (i in 1:length(dts)) {

    dt = dts[i]


    pt = tryCatch({

      t = format(dt, '%H:%M')
      d = format(dt, '%Y-%m-%d')

      ddiff = as.integer(today()) - as.integer(as.Date(d))

      if (d == today()) {
        td = "Today"
      } else if (d == today() - days(1)) {
        td = "Yesterday"
      } else {
        td = sprintf("%i days ago", ddiff)
      }

      pt = sprintf("%s @ %s", td, t)
    },
     error = function(e) {})

    out = c(out, ifelse(length(pt) == 0, "", pt))

  }

  invisible()
  out
}






state_format = function(...) {
  formatter("span", style = function(x) style(display = "block", `text-align` = "center", padding = "1px 4px", `border-radius` = "3px", `background-color` = map_state_color(x)))
}


map_state_color = function(x) {
  out = character()
  for (i in x) {
    col = switch(i,
                 waiting="#AEAEAE",
                 failed="#d9534f",
                 queued = "#f0ad4e",
                 running = "#5bc0de",
                 done = "#5cb85c")
    out = c(out, col)
  }
  out
}

map_state_icon = function(x) {
  out = character()
  for (i in x) {
    col = switch(i,
                 waiting= "pause",
                 failed= "remove",
                 queued = "step-forward",
                 running = "play",
                 done = "ok")
    out = c(out, col)
  }
  out
}


app_triggered_run = function(job_id, dependecies, logging, notification) {
  manual_run_job()
}
