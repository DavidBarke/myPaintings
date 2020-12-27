image_box_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::uiOutput(
    outputId = ns("card")
  )
}

image_box_server <- function(id, .values, image_id, options) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      status_choices <- c("primary", "orange", "olive", "fuchsia")
      
      status <- sample(status_choices, 1)
      
      displays <- list(
        details = bs4Dash::bs4Card(
          width = NULL,
          title = image_box_title_ui(
            id = ns("image_box_title")
          ),
          dropdownMenu = bs4Dash::boxDropdown(
            image_box_dropdown_ui(
              id = ns("image_box_dropdown")
            )
          ),
          solidHeader = TRUE,
          status = status,
          maximizable = TRUE,
          collapsible = FALSE,
          image_box_image_ui(
            id = ns("image_box_image")
          )
        ),
        list = bodyless_card(
          width = NULL,
          title = image_box_title_ui(
            id = ns("image_box_title")
          ),
          dropdownMenu = bs4Dash::boxDropdown(
            image_box_dropdown_ui(
              id = ns("image_box_dropdown")
            )
          ),
          solidHeader = TRUE,
          status = status
        )
      )
      
      output$card <- shiny::renderUI({
        displays[[options$display_r()]]
      })
      
      entry_r <- shiny::reactive({
        .values$update$collection_image_rvs[[as.character(image_id)]]
        db_get_image_entry_by_image_id(.values$db, image_id)
      })
      
      is_offered_r <- shiny::reactive({
        .values$update$offered_images()
        db_is_image_offered(.values$db, image_id)
      })
      
      price_r <- shiny::reactive({
        if (!is_offered_r()) return(NULL)
        
        .values$update$collection_image_rvs[[as.character(image_id)]]
        db_get_offered_price(.values$db, image_id)
      }) 
      
      image_r <- shiny::reactive({
        c(
          entry_r(),
          list(
            is_offered = is_offered_r(),
            price = price_r()
          )
        )
      })
      
      image_box_title_server(
        id = "image_box_title",
        .values = .values,
        image_r = image_r
      )
      
      image_box_dropdown_server(
        id = "image_box_dropdown",
        .values = .values,
        image_r = image_r
      )
      
      image_box_image_server(
        id = "image_box_image",
        .values = .values,
        image_r = image_r
      )
      
      return_list <- list(
        is_offered_r = is_offered_r
      )
      
      return(return_list)
    }
  )
}