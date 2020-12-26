image_box_dropdown_price_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("price_item")
  )
}

image_box_dropdown_price_server <- function(id, .values, image_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$price_item <- shiny::renderUI({
        if (image_r()$is_offered) {
          bs4Dash::cardDropdownItem(
            id = ns("change_price"),
            "Price",
            icon = shiny::icon("edit")
          )
        }
      })
    }
  )
}