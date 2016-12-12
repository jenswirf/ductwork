

shinyUI(

  dashboardPage(skin = "black",

  dashboardHeader(title="ductwork"),

  dashboardSidebar(
    sidebarMenu(
      br(),
      #div(actionButton("refresh", label = "Refresh", icon = icon("refresh", "fa"), width = "200px"),  style = "margin: 0px 14px; padding: 20px 0"),
    menuItem("Jobs", tabName = "jobs", icon = icon("dashboard"), selected = T),
    menuItem("Log", icon = icon("bars"), tabName = "log"),
    menuItem("Stats", icon = icon("bar-chart"), tabName = "stats")

    , br(),


    #sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
    #                 label = "Search..."),
    uiOutput("state"),
    uiOutput("type"),
    uiOutput("author"),
    selectInput("logic", label = "Logic",
                choices = list("Any" = "Any", "Scheduled" = TRUE, "Dependent" = FALSE),
                selected = "Any"),
    uiOutput("group")

)
      ),

  dashboardBody(
     tags$head(tags$link(rel="ductwork", href="www/favicon.ico"),
               tags$script(src = "message-handler.js"),
               tags$style(HTML('


     .wrapper, .main-sidebar, .content-wrapper, .left-side  {
        background-color: #FAFAFA !important;
            }



     .sidebar-menu>li>a {
        background-color: #FAFAFA !important;
        color: #333 !important;
     }
      .sidebar-menu>li>a:hover {
        background-color: rgba(221, 221, 221, 0.3) !important;
        color: #333;
      }

    .github-link {
      display: block;
      padding: 7px 0px 0px 13px;
      font-size: 14px;
      color: black;
    }

    .github-link:hover {
      color: #4078C0;
    }

    .sidebar-menu>li.active>a {
          border-left-color: transparent !important;
          font-weight: bold;
          font-size: 15px;
    }

    .skin-black .sidebar-menu>li:hover>a {
          border-left-color: transparent !important;
    }

      .selectize-input.full {
      background-color: #FAFAFA;
      border-color: #333;
      color: #777 !important;
      }

    .control-label {
      color: #333 !important;
    }

#      .btn-default, .btn-default:hover, .btn-default:focus {
#       background-color: transparent;
#       color: black;
#       border-color: transparent;
#       -webkit-box-shadow: none;
# 	-moz-box-shadow: none;
#       box-shadow: none;
#       outline: 0;
#      }

     .link-icon {
      display:block;
      display: block;
      padding: 7px 0px 0px 13px;
      background-color: transparent;
                               color: black;
                               border-color: transparent;
                               -webkit-box-shadow: none;
                               -moz-box-shadow: none;
                               box-shadow: none;
                               outline: 0;
                               }

  .link-icon:hover {
      background-color: transparent !important;
      color: #5cb85c;
      border-color: transparent;
      -webkit-box-shadow: none;
                    -moz-box-shadow: none;
                    box-shadow: none;
                    outline: 0;
 }


    input[type=text] {
      background-color: #FAFAFA !important;
      border-radius: 3px;
      color: #333 !important;
      border: 1px solid #eee !important;
    }

    .sidebar-form {
       border: 1px solid #eee !important;
       color: #333 !important;
    }

    .sidebar-form .btn, .skin-black .sidebar-form input[type=text] {
       background-color: #FAFAFA !important;
    }

    .selectize-input.full {
      background-color: #FAFAFA;
      border-color: #eee;
      color: #f4f4f4;
    }

    input {
    background-color: #FAFAFA;
    }

    th, td {
      text-align: left !important;
    }

    .box {
      border-top: 3px solid rgba(221, 221, 221, 0.3)
    }

    '))),

    tabItems(
      tabItem(tabName = "jobs",

                box(
                  em("Jobs"),
                  formattableOutput("states"),
                  width = 12
                )
      ),

      tabItem(tabName = "log",
              fluidRow(
                box(width = 12,
                    em("Log"),
                  formattableOutput("log")
                )
              )
      ),
      tabItem(tabName = "stats",


              box(
                em("Timeline Today"),
                plotOutput("timeline", width = "100%"),
                width = 12
              )
      ),
      tabItem(tabName = "manual",


              box(
                em("Manually rerun a job"), br(),br(),
                HTML('<p>Start a command line session on the data-pipe server and run this:</p>'),
                HTML('<span>If the job uses the tictail R package, first run </span><code>source /opt/tictail/data/.profile</code>'),br(),
                HTML('<span>then </span><code id="cmd">Well, this did not work :( </code>'),
                HTML("<script>
                     function write_cmd(job) {
                        document.getElementById('cmd').innerHTML = 'Rscript /opt/tictail/data/utilities/ductwork/manual_run_job.R ' + job;
                     }
                     </script>"),
                br(), br(),
                #checkboxGroupInput("options", label =h6(""),
                #                  choices = list("Also run dependencies" = "depedencies")),
                #uiOutput("ids"), br(),
                #actionButton("run", "Run!"), HTML("&nbsp;&nbsp;"), 
                HTML("<a  href=\"#shiny-tab-jobs\" data-toggle=\"tab\" data-value=\"jobs\">Cancel</a>"),
                width = 12
              )
      )


)

  )
))
