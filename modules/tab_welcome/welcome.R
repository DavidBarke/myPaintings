welcome_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow()
}

welcome_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
    }
  )
}