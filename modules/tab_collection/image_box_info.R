image_box_info_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("info")
  )
}

image_box_info_server <- function(id, .values, image_r, status) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$info <- shiny::renderUI({
        shiny::fluidRow(
          title_ui_r(),
          painter_ui_r(),
          museum_ui_r()
        )
      })
      
      title_ui_r <- shiny::reactive({
        bs4Dash::infoBox(
          title = "Original Title",
          value = title_r(),
          width = 12,
          icon = shiny::icon("heading")
        )
      })
      
      title_r <- shiny::reactive({
        image_r()$original_title %NA% "Missing"
      })
      
      painter_ui_r <- shiny::reactive({
        bs4Dash::infoBox(
          title = "Painter",
          value = painter_r(),
          width = 12,
          icon = shiny::icon("paint-brush")
        )
      })
      
      painter_r <- shiny::reactive({
        image_r()$painter %NA% "Missing" 
      })
      
      museum_ui_r <- shiny::reactive({
        bs4Dash::infoBox(
          title = "Exhibited at",
          value = museum_r(),
          width = 12,
          icon = shiny::icon("landmark")
        )
      })
      
      museum_r <- shiny::reactive({
        format_museum(
          museum = image_r()$museum,
          city = image_r()$city
        )
      })
    }
  )
}