image_box_title_ui <- function(id, image) {
  ns <- shiny::NS(id)
  
  price_badge <- if (image$is_offered) {
    bs4Dash::bs4Badge(
      scales::dollar_format()(image$price),
      color = "danger"
    )
  }
  
  htmltools::tagList(
    image$title,
    price_badge
  )
}