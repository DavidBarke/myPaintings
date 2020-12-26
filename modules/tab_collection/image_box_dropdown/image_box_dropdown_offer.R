image_box_dropdown_offer_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("offer_item")
  )
}

image_box_dropdown_offer_server <- function(id, .values, image_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$offer_item <- shiny::renderUI({
        if (!image_r()$is_offered) {
          bs4Dash::cardDropdownItem(
            id = ns("offer_image"),
            "Offer",
            icon = shiny::icon("money-bill-wave")
          )
        } else {
          bs4Dash::cardDropdownItem(
            id = ns("cancel_offer"),
            "Withdraw offer",
            icon = shiny::icon("ban")
          )
        }
      })
    }
  )
}