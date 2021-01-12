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
      
      type <- "buy"
      
      image_display_server(
        id = "buy",
        .values = .values,
        display_args = list(
          header = list(
            type = type
          ),
          content = list(
            type = type
          )
        )
      )
    }
  )
}