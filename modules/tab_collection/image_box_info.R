image_box_info_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    bs4Dash::infoBox(
      title = "Owner",
      value = shiny::uiOutput(outputId = ns("owner")),
      width = 12,
      icon = shiny::icon("user")
    ),
    bs4Dash::infoBox(
      title = "Painter",
      value = shiny::uiOutput(ns("painter")),
      width = 12,
      icon = shiny::icon("paint-brush")
    ),
    bs4Dash::infoBox(
      title = "Created in",
      value = shiny::uiOutput(ns("date")),
      width = 12,
      icon = shiny::icon("hourglass")
    ),
    bs4Dash::infoBox(
      title = "Exhibited at",
      value = shiny::uiOutput(ns("location")),
      width = 12,
      icon = shiny::icon("landmark")
    ),
    bs4Dash::infoBox(
      title = "School",
      value = shiny::uiOutput(ns("school")),
      width = 12,
      icon = shiny::icon("school")
    ),
    bs4Dash::infoBox(
      title = "Source",
      value = shiny::uiOutput(ns("source")),
      width = 12,
      icon = shiny::icon("copyright")
    )
  )
}

image_box_info_server <- function(id, .values, image_r, status) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$owner <- shiny::renderUI({
        image_r()$owner
      })
      
      output$painter <- shiny::renderUI({
        format_painter(
          first_name = image_r()$first_name,
          last_name = image_r()$last_name
        )
      })
      
      output$date <- shiny::renderUI({
        image_r()$date
      })
      
      output$location <- shiny::renderUI({
        image_r()$location
      })
      
      output$school <- shiny::renderUI({
        image_r()$school
      })
      
      output$source <- shiny::renderUI({
        url <- image_r()$url
        
        htmltools::tags$a(
          href = url,
          target = "_blank",
          url
        )
      })
    }
  )
}