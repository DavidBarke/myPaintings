image_box_title_ui <- function(id, image) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    image$title,
    if (image$is_offered) price_badge(image$price)
  )
}