image_box_dropdown_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    image_box_dropdown_offer_ui(
      id = ns("image_box_dropdown_offer")
    ),
    image_box_dropdown_price_ui(
      id = ns("image_box_dropdown_price")
    )
  )
}

image_box_dropdown_server <- function(id, .values, image_r, box_id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      offer_return <- image_box_dropdown_offer_server(
        id = "image_box_dropdown_offer",
        .values = .values,
        image_r = image_r,
        box_id = box_id
      )
      
      price_return <- image_box_dropdown_price_server(
        id = "image_box_dropdown_price",
        .values = .values,
        image_r = image_r,
        is_offered_r = offer_return$is_offered_r
      )
  
      return_list <- list(
        offer = offer_return,
        price = price_return
      )
    }
  )
}