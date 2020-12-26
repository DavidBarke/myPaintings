image_box_dropdown_offer_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("offer_item")
  )
}

image_box_dropdown_offer_server <- function(id, .values, image_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$offer_item <- shiny::renderUI({
        if (!image_r()$is_offered) {
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
            currencySymbolPlacement = "s"
          ),
          footer = shiny::uiOutput(
            outputId = ns("confirm_offer")
          )
        ))
      })
      
      output$confirm_offer <- shiny::renderUI({
        btn <- shiny::actionButton(
          inputId = ns("confirm_offer"),
          label = "Confirm Offer"
        )
        
        if (offer_error_r()) shinyjs::disabled(btn) else btn
      })
      
      offer_error_r <- shiny::reactive({
        FALSE
      })
      
      shiny::observeEvent(input$confirm_offer, {
        shiny::removeModal()
        
        db_offer_image(
          db = .values$db,
          image_id = image_r()$image_id,
          price = input$price
        )

        .values$update$offered_images(.values$update$offered_images() + 1)
        
        bs4Dash::toast(
          paste0(
            "Offered \"", 
            image_r()$title, 
            "\" for ",
            .values$settings$dollar_format(input$price),
            "."
          ),
          options = .values$settings$toast(
            delay = 3000,
            class = "bg-success"
          )
        )
      })
    }
  )
}