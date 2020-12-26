image_box_dropdown_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("dropdown")
  )
}

image_box_dropdown_server <- function(id, .values, image_id, image_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      is_offered_r <- shiny::reactive({
        .values$update$offered_images()
        db_is_image_offered(.values$db, image_id)
      })
      
      offer_item_r <- shiny::reactive({
        if (!is_offered_r()) {
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
      
      price_item_r <- shiny::reactive({
        if (is_offered_r()) {
          bs4Dash::cardDropdownItem(
            id = ns("change_price"),
            "Price",
            icon = shiny::icon("edit")
          )
        }
      })
      
      output$dropdown <- shiny::renderUI({
        htmltools::tagList(
          offer_item_r(),
          price_item_r(),
          bs4Dash::cardDropdownItem(
            id = ns("change_title"),
            "Title",
            icon = shiny::icon("edit")
          )
        )
      })
      
      shiny::observeEvent(input$change_title, {
        shiny::showModal(shiny::modalDialog(
          title = "Painting Title",
          easyClose = TRUE,
          shiny::textInput(
            inputId = ns("title"),
            label = "Title",
            value = image_r()$title
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_title"),
            label = "Confirm"
          )
        ))
      })
      
      shiny::observeEvent(input$confirm_title, {
        shiny::removeModal()
        
        db_set_image_title(.values$db, image_id, input$title)
        .values$update$collection_image_rvs[[as.character(image_id)]] <- runif(1)
      })
    }
  )
}