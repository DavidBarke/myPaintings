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
          painter_ui_r(),
          location_ui_r()
        )
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
        format_painter(
          first_name = image_r()$first_name,
          last_name = image_r()$last_name
        )
      })
      
      location_ui_r <- shiny::reactive({
        bs4Dash::infoBox(
          title = "Exhibited at",
          value = location_r(),
          width = 12,
          icon = shiny::icon("landmark")
        )
      })
      
      location_r <- shiny::reactive({
        image_r()$location
      })
    }
  )
}