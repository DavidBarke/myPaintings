welcome_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    bs4Dash::bs4Card(
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      title = "User",
      DT::dataTableOutput(
        outputId = ns("user_table")
      )
    )
  )
}

welcome_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$user_table <- DT::renderDataTable({
        user_ids <- db_get_user_ids(.values$db)
        
        tbl <- tibble::tibble(
          Name = names(user_ids)
        )
        
        DT::datatable(tbl)
      })
    }
  )
}