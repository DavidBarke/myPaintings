image_display_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    image_display_header_ui(
      id = ns("image_display_header")
    ),
    display_results_number_ui(
      id = ns("image_display_results_number")
    ),
    image_display_content_ui(
      id = ns("image_display_content")
    )
  )
}

image_display_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      header_return <- image_display_header_server(
        id = "image_display_header",
        .values = .values
      )
      
      display_results_number_server(
        id = "image_display_results_number",
        .values = .values,
        n_r = header_return$n_r
      )
      
      image_display_content_server(
        id = "image_display_content",
        .values = .values,
        options = header_return
      )
    }
  )
}