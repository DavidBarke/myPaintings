library(shiny)
library(bs4Dash)
library(shinyjs)
library(dplyr)
library(stringr)
library(qrcode)
library(purrr)
library(DT)
library(lubridate)
library(bcrypt)
library(shinycssloaders)

ui_server <- function(source_to_globalenv = FALSE) {
  # If source_to_global_env all sourced functions get added to the global
  # environment which takes some time after the app has stopped
  
  source("init/source_directory.R")
  
  source_directory(
    # chdir enables use of relative paths in source statements inside
    # these sourced files
    path = "./modules",
    encoding = "UTF-8",
    modifiedOnly = FALSE,
    chdir = TRUE,
    recursive = TRUE,
    envir = if (source_to_globalenv) globalenv() else environment()
  )
  
  source_directory(
    path = "./db/interface",
    encoding = "UTF-8",
    modifiedOnly = FALSE,
    chdir = TRUE,
    recursive = TRUE,
    envir = if (source_to_globalenv) globalenv() else environment()
  )
  
  # Globals ------------------------------------------------------------------
  
  # Allow bigger file inputs
  options(shiny.maxRequestSize = 100*1024^2)
  
  # modules/dt_options.R
  dt_options()
  
  # UI -----------------------------------------------------------------------
  ui <- htmltools::div(
    tags$head(
      # Include custom css styles
      shiny::includeCSS("www/css/styles.css"),
      shiny::includeHTML("www/html/ga.html")
    ),
    container_ui(
      id = "container"
    ),
    # Enable shinyjs
    useShinyjs(),
    # Extend shinyjs with custom JavaScript
    extendShinyjs(
      "js/extend_shinyjs.js", 
      functions = c("disable_selectize_input", "scroll_trigger")
    )
  )
  
  # SERVER -------------------------------------------------------------------
  
  server <- function(input, output, session) {
    
    shiny::addResourcePath(prefix = "wga", directoryPath = "./img/wga")
    
    # .VALUES ENVIRONMENT ------------------------------------------------
    
    # The .values environment is available to all modules so that arbitrary information
    # can be shared via this environment. Elements that underly reactive changes can be
    # stored as reactiveValues or reactiveVal
    .values <- new.env()
    # Set a value to .values$trigger$<value> inside a module and listen to its
    # change in some other module with observeEvent(.values$trigger$<value>, ...)
    .values$trigger <- shiny::reactiveValues()
    # Same purpose as above, but you must set the reactiveValues by yourself. This
    # is useful for modules that get reused multiple times and therefore can
    # store a trigger for each instance
    .values$trigger_list <- list()
    
    .values$settings$password$length <- list(min = 4, max = 16)
    .values$settings$user_name$length <- list(min = 4, max = 16)
    .values$settings$group_name$length <- list(min = 4, max = 16)
    .values$settings$type_name$length <- list(min = 4, max = 16)
    .values$settings$status_dict <- c(
      admin = "Administrator",
      user = "User"
    )
    .values$settings$time_unit_dict <- c(
      secs = "Seconds",
      mins = "Minutes",
      hours = "Hours",
      days = "Days"
    )
    .values$settings$toast <- function(...) {
      dots <- list(...)
      
      default <- list(
        autohide = TRUE,
        delay = 3000,
        position = "bottomRight"
      )
      
      c(dots, default)
    }
    
    .values$update$db_user_rv <- shiny::reactiveVal(0)
    .values$update$db_offered_images_rv <- shiny::reactiveVal(0)
    
    # Connect to db
    .values$db <- DBI::dbConnect(RSQLite::SQLite(), "./db/db.sqlite")
    .values$user_rvs <- shiny::reactiveValues(
      user_id = NULL,
      name = "",
      status = "not_logged",
      password = NULL,
      added_from = NULL,
      time_added = NULL,
      time_logged = NULL,
      times_logged = NULL,
      last_logged = "",
      capital = NULL
    )
    
    # Enable regex on db
    RSQLite::initRegExp(.values$db)
    
    container_server(
      id = "container",
      .values = .values
    )
    
    session$onSessionEnded(function() {
      DBI::dbDisconnect(.values$db)
    })
  }
  
  return(list(ui = ui, server = server))
}

ui_server <- ui_server(source_to_globalenv = FALSE)

ui <- ui_server$ui
server <- ui_server$server

shiny::shinyApp(ui, server)
