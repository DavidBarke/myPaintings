image_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shinydashboard::box(
    width = NULL,
    title = shiny::uiOutput(
      outputId = ns("title")
    ),
    solidHeader = TRUE,
    status = "primary",
    shiny::imageOutput(
      outputId = ns("img"),
      height = NULL,
      click = clickOpts(
        id = ns("img_click")
      )
    )
  )
}

image_server <- function(id, .values, img) {
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
      
      shiny::observeEvent(input$img_click, {
        str(input$img_click)
      })
    }
  )
}