welcome_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    title_box(
      title = "myPaintings",
      subtitle = "Collect and trade paintings from the 13th to the 19th century.",
      width = 12,
      status = "primary"
    ),
    bs4Dash::tabBox(
      id = ns("welcome"),
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      title = NULL,
      shiny::tabPanel(
        title = "Overview",
        welcome_info_ui(
          id = ns("info")
        )
      ),
      shiny::tabPanel(
        title = "User Table",
        welcome_user_table_ui(
          id = ns("user_table")
        )
      ),
      shiny::tabPanel(
        title = "About",
        welcome_about_ui(
          id = ns("about")
        )
      )
    )
  )
}

welcome_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      welcome_info_server(
        id = "info",
        .values = .values
      )
      
      welcome_user_table_server(
        id = "user_table",
        .values = .values
      )
      
      welcome_about_server(
        id = "about",
        .values = .values
      )
    }
  )
}