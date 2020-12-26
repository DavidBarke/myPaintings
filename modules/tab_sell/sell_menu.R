sell_menu_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    bs4Dash::infoBox(
      title = shiny::actionLink(
        inputId = ns("sell_image"),
        label = "Offer Painting for Sale"
      ),
      color = "primary",
      icon = shiny::icon("box-open")
    )
  )
}

sell_menu_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      shiny::observeEvent(input$sell_image, {
        shiny::showModal(shiny::modalDialog(
          title = "Offer painting for sale",
          easyClose = TRUE,
          footer = shiny::actionButton(
            inputId = ns("offer_image"),
            label = "Offer for sale"
          )
        ))
      })
    }
  )
}