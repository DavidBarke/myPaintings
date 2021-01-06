image_box_title_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("title")
  )
}

image_box_title_server <- function(id, .values, image_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      price_badge_r <- shiny::reactive({
        if (image_r()$is_offered) {
          bs4Dash::bs4Badge(
            .values$settings$dollar_format(image_r()$price),
            color = "danger"
          )
        }
      })
      
      output$title <- shiny::renderUI({
        htmltools::tagList(
          paste(image_r()$title, image_r()$index),
          price_badge_r()
        )
      })
    }
  )
}