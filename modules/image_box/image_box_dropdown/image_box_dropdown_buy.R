image_box_dropdown_buy_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bs4Dash::cardDropdownItem(
    id = ns("buy_image"),
    "Buy",
    icon = shiny::icon("hand-holding-usd")
  )
}

image_box_dropdown_buy_server <- function(id, 
                                          .values,
                                          image_r,
                                          price_badge_id,
                                          click_badge_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
    }
  )
}