image_box_dropdown_ui <- function(id, type) {
  ns <- shiny::NS(id)
  
  dropdown_elements <- list(
    buy = image_box_dropdown_buy_ui(
      id = ns("image_box_dropdown_buy")
    ),
    offer = image_box_dropdown_offer_ui(
      id = ns("image_box_dropdown_offer")
    ),
    price = image_box_dropdown_price_ui(
      id = ns("image_box_dropdown_price")
    )
  )

  dropdown_elements_dict <- list(
    browse = c("offer", "price"),
    buy = c("buy"),
    collection = c("offer", "price")
  )
  
  htmltools::tagList(
    unname(dropdown_elements[dropdown_elements_dict[[type]]])
  )
}

image_box_dropdown_server <- function(id, 
                                      .values, 
                                      image_r, 
                                      box_id, 
                                      price_badge_id,
                                      click_badge_r,
                                      type
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      buy_return <- image_box_dropdown_buy_server(
        id = "image_box_dropdown_buy",
        .values = .values,
        image_r = image_r,
        price_badge_id = price_badge_id,
        click_badge_r = click_badge_r,
        type = type
      )
      
      offer_return <- image_box_dropdown_offer_server(
        id = "image_box_dropdown_offer",
        .values = .values,
        image_r = image_r,
        box_id = box_id,
        price_rv = price_return$price_rv,
        price_badge_id = price_badge_id,
        type = type
      )
      
      price_return <- image_box_dropdown_price_server(
        id = "image_box_dropdown_price",
        .values = .values,
        image_r = image_r,
        is_offered_r = offer_return$is_offered_r,
        box_id = box_id,
        price_badge_id = price_badge_id,
        click_badge_r = click_badge_r,
        type = type
      )
  
      return_list <- list(
        offer = offer_return,
        price = price_return,
        type = type
      )
    }
  )
}