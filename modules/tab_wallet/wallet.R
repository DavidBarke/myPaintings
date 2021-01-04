wallet_ui <- function(id) {
  ns <- shiny::NS(id)
  
  display_ui(
    id = ns("transaction_display"),
    header_ui = transaction_display_header_ui,
    results_number_ui = display_results_number_ui,
    content_ui = transaction_display_content_ui
  )
}

wallet_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      display_server(
        id = "transaction_display",
        .values = .values,
        header_server = transaction_display_header_server,
        results_number_server = display_results_number_server,
        content_server = transaction_display_content_server
      )
    }
  )
}