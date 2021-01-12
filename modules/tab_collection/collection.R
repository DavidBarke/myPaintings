collection_ui <- function(id) {
  ns <- shiny::NS(id)
  
  image_display_ui(
    id = ns("collection")
  )
}

collection_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      tab <- "collection"
      
      image_display_server(
        id = "collection",
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