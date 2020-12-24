user_management_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      add_user_ui(
        id = ns("add_user")
      )
    ),
    shiny::column(
      width = 6,
      user_table_ui(
        id = ns("user_table")
      )
    )
  )
}

user_management_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      add_user_server(
        id = "add_user",
        .values = .values
      )

      user_table_server(
        id = "user_table",
        .values = .values
      )
    }
  )
}
