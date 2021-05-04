welcome_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::fluidRow(
      title_box(
        title = "myPaintings",
        subtitle = "Collect and trade paintings from the 13th to the 19th century.",
        width = 12,
        status = "primary"
      )
    ),
    image_display_ui(
      id = ns("browse")
    )
  )
}

welcome_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      welcome_info_server(
        id = "info",
        .values = .values
      )
      
      welcome_README_server(
        id = "readme",
        .values = .values
      )
      
      welcome_user_table_server(
        id = "user_table",
        .values = .values
      )
      
      welcome_about_server(
        id = "about",
        .values = .values
      )
      
      type <- "browse"
      
      image_display_server(
        id = "browse",
        .values = .values,
        display_args = list(
          header = list(
            type = type,
            display = FALSE
          ),
          content = list(
            type = type
          )
        )
      )
    }
  )
}