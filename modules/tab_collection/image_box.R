image_box_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bs4Dash::bs4Card(
    width = 3,
    title = shiny::uiOutput(
      outputId = ns("title")
    ),
    solidHeader = TRUE,
    status = "primary",
    maximizable = TRUE,
    shiny::imageOutput(
      outputId = ns("img"),
      height = NULL
    )
  )
}

image_box_server <- function(id, .values, img) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$title <- shiny::renderUI({
        img$title
      })
      
      output$img <- shiny::renderImage(deleteFile = FALSE, {
        list(
          src = img$src,
          width = "100%",
          height = "auto"
        )
      })
    }
  )
}