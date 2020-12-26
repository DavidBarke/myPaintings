image_box_dropdown_price_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("price_item")
  )
}

image_box_dropdown_price_server <- function(id, .values, image_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$price_item <- shiny::renderUI({
        if (image_r()$is_offered) {
          bs4Dash::cardDropdownItem(
            id = ns("change_price"),
            "Price",
            icon = shiny::icon("edit")
          )
        }
      })
      
      shiny::observeEvent(input$change_price, {
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
            value = image_r()$price,
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
        
        .values$update$offered_images(.values$update$offered_images() + 1)
        
        bs4Dash::toast(
          paste0(
            "Changed price for \"", 
            image_r()$title, 
            "\" to ",
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