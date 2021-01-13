welcome_about_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "container",
    shiny::includeMarkdown("./md/about.md")
  )
}

welcome_about_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
    }
  )
}