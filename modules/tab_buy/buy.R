buy_ui <- function(id) {
  ns <- shiny::NS(id)
  
  image_display_ui(
    id = ns("image_display")
  )
}

buy_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      image_display_server(
        id = "image_display",
        .values = .values,
        display_args = list(
          header = list(
            tab = "buy"
          ),
          content = list()
        )
      )
    }
  )
}