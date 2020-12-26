collection_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    collection_header_ui(
      id = ns("collection_header")
    ),
    collection_images_ui(
      id = ns("collection_images")
    )
  )
}

collection_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      collection_header_return <- collection_header_server(
        id = "collection_header",
        .values = .values
      )
      
      collection_images_server(
        id = "collection_images",
        .values = .values,
        options = collection_header_return
      )
    }
  )
}