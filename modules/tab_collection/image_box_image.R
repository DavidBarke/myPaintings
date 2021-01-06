image_box_image_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::imageOutput(
    outputId = ns("img"),
    height = NULL
  ) #%>% shinycssloaders::withSpinner()
}

image_box_image_server <- function(id, .values, image_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$img <- shiny::renderImage(deleteFile = FALSE, {
        list(
          src = image_r()$path,
          width = "100%",
          height = "auto"
        )
      })
    }
  )
}