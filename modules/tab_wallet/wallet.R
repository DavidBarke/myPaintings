wallet_ui <- function(id) {
  ns <- shiny::NS(id)
  
  transaction_display_ui(
    id = ns("transaction_display")
  )
}

wallet_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      transaction_display_server(
        id = "transaction_display",
        .values = .values
      )
    }
  )
}