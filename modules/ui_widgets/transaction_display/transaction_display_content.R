transaction_display_content_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    bs4Dash::box(
      title = "Transactions",
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      DT::dataTableOutput(
        outputId = ns("transaction_table")
      )
    )
  )
}

transaction_display_content_server <- function(id, .values, options) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$transaction_table <- DT::renderDataTable({
        tbl <- options$transactions_r()
        
        DT::datatable(tbl) %>%
          DT::formatCurrency(
            columns = "Price"
          )
      })
    }
  )
}