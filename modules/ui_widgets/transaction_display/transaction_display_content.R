transaction_display_content_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
  
  )
}

transaction_display_content_server <- function(id, .values, options) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
    }
  )
}