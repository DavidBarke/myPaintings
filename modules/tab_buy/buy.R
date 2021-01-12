buy_ui <- function(id) {
  ns <- shiny::NS(id)
  
  image_display_ui(
    id = ns("buy")
  )
}

buy_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      tab <- "buy"
      
      image_display_server(
        id = "buy",
        .values = .values,
        display_args = list(
          header = list(
            tab = tab
          ),
          content = list(
            tab = tab
          )
        )
      )
    }
  )
}