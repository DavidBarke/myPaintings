image_box_info_ui <- function(id, image) {
  ns <- shiny::NS(id)
  
  if (image$image_id == 1) print(image)
  
  shiny::fluidRow(
    bs4Dash::infoBox(
      title = "Owner",
      value = image$owner,
      width = 12,
      icon = shiny::icon("user")
    ),
    bs4Dash::infoBox(
      title = "Painter",
      value = image$painter,
      width = 12,
      icon = shiny::icon("paint-brush")
    ),
    bs4Dash::infoBox(
      title = "Created in",
      value = image$date,
      width = 12,
      icon = shiny::icon("hourglass")
    ),
    bs4Dash::infoBox(
      title = "Exhibited at",
      value = image$location,
      width = 12,
      icon = shiny::icon("landmark")
    ),
    bs4Dash::infoBox(
      title = "School",
      value = image$school,
      width = 12,
      icon = shiny::icon("school")
    ),
    bs4Dash::infoBox(
      title = "Source",
      value = htmltools::tags$a(
        href = image$url,
        target = "_blank",
        image$url
      ),
      width = 12,
      icon = shiny::icon("copyright")
    )
  )
}