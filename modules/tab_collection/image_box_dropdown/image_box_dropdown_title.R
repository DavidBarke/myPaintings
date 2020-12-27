image_box_dropdown_title_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bs4Dash::cardDropdownItem(
    id = ns("change_title"),
    "Title",
    icon = shiny::icon("edit")
  )
}

image_box_dropdown_title_server <- function(id, .values, image_r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
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
        
        image_id <- image_r()$image_id
        
        db_set_image_title(.values$db, image_id, input$title)
        .values$update$collection_image_rvs[[as.character(image_id)]] <- runif(1)
      })
    }
  )
}