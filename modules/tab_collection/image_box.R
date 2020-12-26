image_box_ui <- function(id) {
  ns <- shiny::NS(id)
  
  status_choices <- c("primary", "orange", "olive", "fuchsia")
  
  status <- sample(status_choices, 1)
  
  bs4Dash::bs4Card(
    width = NULL,
    title = shiny::uiOutput(
      outputId = ns("title")
    ),
    dropdownMenu = bs4Dash::cardDropdown(
      bs4Dash::cardDropdownItem(
        id = ns("change_title"),
        "Title",
        icon = shiny::icon("edit")
      )
    ),
    solidHeader = TRUE,
    status = status,
    maximizable = TRUE,
    shiny::imageOutput(
      outputId = ns("img"),
      height = NULL
    )
  )
}

image_box_server <- function(id, .values, image_id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      image_r <- shiny::reactive({
        .values$update$collection_image_rvs[[as.character(image_id)]]
        db_get_image_entry_by_image_id(.values$db, image_id)
      })
      
      output$title <- shiny::renderUI({
        image_r()$title
      })
      
      output$img <- shiny::renderImage(deleteFile = FALSE, {
        list(
          src = image_r()$path,
          width = "100%",
          height = "auto"
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