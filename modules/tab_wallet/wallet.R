wallet_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow()
}

wallet_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
    }
  )
}