image_box_ui <- function(id) {
  ns <- shiny::NS(id)
  
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
    status = "primary",
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
        print("Change")
      })
    }
  )
}