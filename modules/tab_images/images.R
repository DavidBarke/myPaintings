images_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      bs4Dash::box(
        width = NULL,
        title = "Images",
        status = "primary",
        solidHeader = TRUE
      )
    ),
    shiny::column(
      width = 2,
      image_ui(
        id = ns("mona_lisa")
      )
    )
  )
}

images_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      image_server(
        id = "mona_lisa",
        img = list(
          src = "./img/mona_lisa.png",
          title = "Mona Lisa"
        )
      )
    }
  )
}