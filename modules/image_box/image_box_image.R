image_box_image_ui <- function(id, src) {
  ns <- shiny::NS(id)
  
  htmltools::img(
    src = src,
    width = "100%",
    height = "auto"
  )
}