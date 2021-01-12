image_box_dropdown_offer_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("offer_item")
  )
}

image_box_dropdown_offer_server <- function(id, 
                                            .values, 
                                            image_r, 
                                            box_id, 
                                            # from image_box_dropdown_price
                                            price_rv,
                                            price_badge_id,
                                            type
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      is_offered_rv <- shiny::reactiveVal(NULL)
      
      shiny::observeEvent(image_r(), {
        is_offered_rv(image_r()$is_offered)
      })
      
      output$offer_item <- shiny::renderUI({
        if (!is_offered_rv()) {
          bs4Dash::cardDropdownItem(
            id = ns("offer_image"),
            "Offer",
            icon = shiny::icon("money-bill-wave")
          )
        } else {
          bs4Dash::cardDropdownItem(
            id = ns("cancel_offer"),
            "Withdraw offer",
            icon = shiny::icon("ban")
          )
        }
      })
      
      

      ## Offer ----   
      shiny::observeEvent(input$offer_image, {
        shiny::showModal(shiny::modalDialog(
          easyClose = TRUE,
          title = "Offer Painting",
          shinyWidgets::autonumericInput(
            inputId = ns("price"),
            label = "Price",
            value = 1000,
            minimumValue = 0,
            currencySymbol = " $",
            currencySymbolPlacement = "p"
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_offer"),
            label = "Confirm Offer"
          )
        ))
      })
      
      shiny::observeEvent(input$confirm_offer, {
        shiny::removeModal()
        
        db_offer_image(
          db = .values$db,
          image_id = image_r()$image_id,
          price = input$price
        )
        
        shiny::insertUI(
          selector = paste0("#", box_id, " .card-title"),
          where = "beforeEnd",
          ui = price_badge(input$price, price_badge_id)
        )

        .values$update$db_offered_images_rv(.values$update$db_offered_images_rv() + 1)
        is_offered_rv(TRUE)
        price_rv(input$price)
        
        bs4Dash::toast(
          paste0(
            "Offered \"",
            image_r()$title,
            "\" for ",
            scales::dollar_format()(input$price),
            "."
          ),
          options = .values$settings$toast(
            delay = 3000,
            class = "bg-success"
          )
        )
      })
      
      
      
      ## Withdraw offer ----
      shiny::observeEvent(input$cancel_offer, {
        db_withdraw_offer_image(
          db = .values$db,
          image_id = image_r()$image_id
        )
        
        shiny::removeUI(
          selector = paste0("#", box_id, " .price-badge")
        )
        
        .values$update$db_offered_images_rv(.values$update$db_offered_images_rv() + 1)
        is_offered_rv(FALSE)
        
        bs4Dash::toast(
          paste0(
            "Withdrew offer for \"",
            image_r()$title,
            "\"."
          ),
          options = .values$settings$toast(
            delay = 3000,
            class = "bg-success"
          )
        )
      })
      
      return_list <- list(
        is_offered_r = shiny::reactive(is_offered_rv())
      )
      
      return(return_list)
    }
  )
}