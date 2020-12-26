image_box_ui <- function(id) {
  ns <- shiny::NS(id)
  
  status_choices <- c("primary", "orange", "olive", "fuchsia")
  
  status <- sample(status_choices, 1)
  
  bs4Dash::bs4Card(
    width = NULL,
    title = shiny::uiOutput(
      outputId = ns("title")
    ),
    dropdownMenu = bs4Dash::boxDropdown(
      image_box_dropdown_ui(
        id = ns("image_box_dropdown")
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
      
      image_box_dropdown_server(
        id = "image_box_dropdown",
        .values = .values,
        image_r = image_r
      )
      
      is_offered_r <- shiny::reactive({
        .values$update$offered_images()
        db_is_image_offered(.values$db, image_id)
      })
      
      entry_r <- shiny::reactive({
        .values$update$collection_image_rvs[[as.character(image_id)]]
        db_get_image_entry_by_image_id(.values$db, image_id)
      })
      
      image_r <- shiny::reactive({
        c(
          entry_r(),
          list(
            is_offered = is_offered_r()
          )
        )
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
    }
  )
}