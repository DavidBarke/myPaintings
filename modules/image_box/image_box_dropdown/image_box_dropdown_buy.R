image_box_dropdown_buy_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bs4Dash::cardDropdownItem(
    id = ns("buy_image"),
    "Buy",
    icon = shiny::icon("hand-holding-usd")
  )
}

image_box_dropdown_buy_server <- function(id, 
                                          .values,
                                          image_r,
                                          price_badge_id,
                                          click_badge_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      modal_buy_rv <- shiny::reactiveVal(0)
      
      shiny::observeEvent(input$buy_image, {
        modal_buy_rv(modal_buy_rv() + 1)
      })
      
      shiny::observeEvent(click_badge_r(), {
        modal_buy_rv(modal_buy_rv() + 1)
      })
      
      shiny::observeEvent(modal_buy_rv(), ignoreInit = TRUE, {
        shiny::showModal(shiny::modalDialog(
          easyClose = TRUE,
          title = paste("Buy", image_r()$title),
          paste(
            "Please confirm that you want to buy",
            image_r()$title,
            "from",
            image_r()$owner,
            "for a price of",
            scales::dollar_format()(image_r()$price)
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm"),
            label = "Confirm"
          )
        ))
      })
      
      shiny::observeEvent(input$confirm, {
        shiny::removeModal()
      })
    }
  )
}