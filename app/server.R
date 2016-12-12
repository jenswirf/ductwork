library(shinydashboard)
source("global.R")


shinyServer(function(input, output, session) {


  output$states <- renderFormattable(
    state_table(input)
    )

  output$log <- renderFormattable(
      log_table()
      )

  output$timeline <- renderPlot({
    stats_timeline()}
    )

  jobs = read_jobs(config$statekeeper)
  
  output$type <- renderUI({
    types = jobs %>% select(type) %>% distinct %>% unlist %>% unname
    selectInput("type", choices = as.list(c("Any", types)), label = "Type", selected = "Any")
  })

  output$state <- renderUI({
    states = jobs %>% select(state) %>% distinct %>% unlist %>% unname
    selectInput("state", choices = as.list(c("Any", states)), label = "State", selected = "Any")
  })

  output$author <- renderUI({
    authors = jobs %>% filter(!is.na(author)) %>% select(author) %>% distinct %>% unlist %>% unname
    selectInput("author", choices = as.list(c("Anyone", authors)), label = "Author", selected = "Anyone")
  })

  output$group <- renderUI({
    groups = jobs %>% filter(!is.na(grp)) %>% select(grp) %>% distinct %>% unlist %>% unname
    selectInput("group", choices = as.list(c("Any", groups)), label = "Group", selected = "Any")
  })

  output$ids <- renderUI({
    ids = jobs %>% select(id) %>% distinct %>% unlist %>% unname
    selectInput("ids", choices = as.list(ids), label = "Job to run:")
  })


  output$status <- renderText({
    statustext()
  })

  # observeEvent(input$run, {
  # 
  #   job_id = input$ids
  #   dependecies = ifelse("dependencies" %in% input$options, T, F)
  #   manual_run_job(job_id)
  # 
  # })

})




