welcome_user_table_ui <- function(id) {
  ns <- shiny::NS(id)
  
  DT::dataTableOutput(
    outputId = ns("user_table")
  )
}

welcome_user_table_server <- function(id, .values) {
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