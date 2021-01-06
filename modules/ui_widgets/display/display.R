display_ui_factory <- function(
  header_ui,
  results_number_ui,
  content_ui
) {
  function(id) {
    ns <- shiny::NS(id)
    
    htmltools::tagList(
      header_ui(
        id = ns("display_header")
      ),
      results_number_ui(
        id = ns("display_results_number")
      ),
      content_ui(
        id = ns("display_content")
      )
    )
  }
}

display_server_factory <- function(
  header_server,
  results_number_server,
  content_server,
  display_args
) {
  function(id, .values) {
    shiny::moduleServer(
      id,
      function(input, output, session) {
        
        ns <- session$ns
        
        header_return <- header_server(
          id = "display_header",
          .values = .values,
          display_args = display_args$header
        )
        
        results_number_server(
          id = "display_results_number",
          .values = .values,
          n_r = header_return$n_r
        )
        
        content_server(
          id = "display_content",
          .values = .values,
          display_args = display_args$content,
          options = header_return
        )
      }
    )
  }
}