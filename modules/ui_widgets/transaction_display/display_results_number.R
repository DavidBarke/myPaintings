display_results_number_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
  
  )
}

display_results_number_server <- function(id, .values, n_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
    }
  )
}