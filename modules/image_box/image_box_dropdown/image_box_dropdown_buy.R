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
                                          click_badge_r,
                                          type
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
        has_enough_money <- .values$user_rvs$capital > image_r()$price
        
        dialog <- if (has_enough_money) {
          shiny::modalDialog(
            easyClose = TRUE,
            title = paste("Buy", image_r()$title),
            paste0(
              "Please confirm that you want to buy \"",
              image_r()$title,
              "\" from ",
              image_r()$owner,
              " for a price of ",
              scales::dollar_format()(image_r()$price),
              "."
            ),
            footer = shiny::actionButton(
              inputId = ns("confirm"),
              label = "Confirm"
            )
          )
        } else {
          shiny::modalDialog(
            easyClose = TRUE,
            title = "Transaction denied",
            "You don't have enough money to buy this image."
          )
        }
        
        shiny::showModal(dialog)
      })
      
      shiny::observeEvent(input$confirm, {
        shiny::removeModal()
        
        db_buy_image(
          db = db,
          image_id = image_r()$image_id,
          buyer_id = .values$user_rvs$user_id
        )
        
        capital <- db_get_user_capital(.values$db, .values$user_rvs$user_id)
        .values$user_rvs$capital <- capital
        
        .values$update$db_user_rv(.values$update$db_user_rv() + 1)
        
        bs4Dash::toast(
          paste(
            "Bought \"",
            image_r()$title,
            "\" for a price of ",
            scales::dollar_format()(image_r()$price),
            "."
          ),
          options = .values$settings$toast(
            class = "bg-success"
          )
        )
      })
    }
  )
}