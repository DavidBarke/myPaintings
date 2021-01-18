welcome_README_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::div(
    class = "container",
    htmltools::p(
      "This README is also available here:",
      htmltools::a(
        "https://github.com/DavidBarke/myPaintings#mypaintings",
        href = "https://github.com/DavidBarke/myPaintings#mypaintings"
      )
    ),
    shiny::includeMarkdown("README.md")
  )
}

welcome_README_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
    }
  )
}