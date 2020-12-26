buy_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
  
  )
}

buy_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
    }
  )
}