display_results_number_ui <- function(id) {
  ns <- shiny::NS(id)
  
  
  shiny::fluidRow(
    width = 12,
    shiny::column(
      width = 12,
      shiny::uiOutput(
        outputId = ns("results")
      )
    )
  )
}

display_results_number_server <- function(id, .values, n_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$results <- shiny::renderUI({
        paste(n_r(), "results")
      })
    }
  )
}