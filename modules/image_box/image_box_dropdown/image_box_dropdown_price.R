image_box_dropdown_price_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("price_item")
  )
}

image_box_dropdown_price_server <- function(id, 
                                            .values, 
                                            image_r, 
                                            is_offered_r, 
                                            box_id,
                                            price_badge_id,
                                            click_badge_r,
                                            type
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      price_rv <- shiny::reactiveVal()
      
      shiny::observeEvent(image_r(), {
        price_rv(image_r()$price)
      })
      
      output$price_item <- shiny::renderUI({
        if (is_offered_r()) {
          bs4Dash::cardDropdownItem(
            id = ns("change_price"),
            "Price",
            icon = shiny::icon("edit")
          )
        }
      })
      
      change_price_rv <- shiny::reactiveVal(0)
      
      shiny::observeEvent(input$change_price, {
        change_price_rv(change_price_rv() + 1)
      })
      
      shiny::observeEvent(click_badge_r(), {
        change_price_rv(change_price_rv() + 1)
      })
      
      shiny::observeEvent(change_price_rv(), ignoreInit = TRUE, {
        shiny::showModal(shiny::modalDialog(
          easyClose = TRUE,
          title = paste0(
            "Change price of \"",
            image_r()$title,
            "\""
          ),
          shinyWidgets::autonumericInput(
            inputId = ns("price"),
            label = "Price",
            value = price_rv(),
            minimumValue = 0,
            currencySymbol = " $",
            currencySymbolPlacement = "p"
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_price"),
            label = "Confirm"
          )
        ))
      })
      
      shiny::observeEvent(input$confirm_price, {
        shiny::removeModal()
        
        db_set_offered_price(
          db = .values$db,
          image_id = image_r()$image_id,
          price = input$price
        )
        
        shiny::removeUI(
          selector = paste0("#", box_id, " .price-badge")
        )
        
        shiny::insertUI(
          selector = paste0("#", box_id, " .card-title"),
          where = "beforeEnd",
          ui = price_badge(input$price, price_badge_id)
        )
        
        price_rv(input$price)
        .values$update$db_offered_images_rv(.values$update$db_offered_images_rv() + 1)
        
        bs4Dash::toast(
          paste0(
            "Changed price for \"", 
            image_r()$title, 
            "\" to ",
            scales::dollar_format()(input$price),
            "."
          ),
          options = .values$settings$toast(
            delay = 3000,
            class = "bg-success"
          )
        )
      })
      
      return_list <- list(
        price_rv = price_rv
      )
      
      return(return_list)
    }
  )
}