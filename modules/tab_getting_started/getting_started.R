getting_started_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    bs4Dash::box(
      width = 12,
      title = "Getting Started",
      solidHeader = TRUE,
      status = "primary",
      collapsible = FALSE,
      welcome_info_ui(
        id = ns("welcome_info")
      )
    ),
    bs4Dash::box(
      width = 12,
      title = "User table",
      solidHeader = TRUE,
      status = "primary",
      collapsible = FALSE,
      welcome_user_table_ui(
        id = ns("welcome_user_table")
      )
    )
  )
}

getting_started_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      welcome_info_server(
        id = "welcome_info",
        .values = .values
      )
      
      welcome_user_table_server(
        id = "welcome_user_table",
        .values = .values
      )
    }
  )
}