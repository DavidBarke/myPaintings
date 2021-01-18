welcome_info_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "container",
    shiny::includeMarkdown("./md/welcome.md")
  )
}

welcome_info_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
    }
  )
}