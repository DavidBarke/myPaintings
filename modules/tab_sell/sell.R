sell_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    sell_menu_ui(
      id = ns("sell_menu")
    ),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        sell_image_box_ui(
          id = ns("price_image")
        )
      )
    )
  )
}

sell_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      sell_menu_server(
        id = "sell_menu",
        .values = .values
      )
      
      sell_image_box_server(
        id = "price_image",
        .values = .values
      )
    }
  )
}