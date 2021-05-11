welcome_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    shiny::fluidRow(
      title_box(
        title = "myPaintings",
        subtitle = htmltools::tagList(
          htmltools::p(
            "Collect and trade paintings from the 13th to the 19th century."
          ),
          shiny::actionButton(
            inputId = ns("getting_started"),
            label = "Getting Started",
            icon = shiny::icon("rocket")
          ),
          shiny::actionButton(
            inputId = ns("login_random"),
            label = "Login as random user",
            icon = shiny::icon("random")
          )
        ),
        width = 12,
        status = "primary"
      )
    ),
    image_display_ui(
      id = ns("browse")
    )
  )
}

welcome_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      shiny::observeEvent(input$getting_started, {
        .values$update_sidebar("getting_started")
      })
      
      shiny::observeEvent(input$login_random, {
        user_ids <- db_get_user_ids(.values$db)
        user_id <- sample(user_ids, 1)
        entry <- db_get_user_entry(.values$db, user_id)
        
        purrr::walk2(names(entry), entry, function(name, value) {
          .values$user_rvs[[name]] <- value
        })
        
        user_name <- db_get_user_name(.values$db, user_id)
        
        db_log_user_in(.values$db, user_name)
        .values$update$db_user_rv(.values$update$db_user_rv() + 1)
      })
      
      type <- "browse"
      
      image_display_server(
        id = "browse",
        .values = .values,
        display_args = list(
          header = list(
            type = type,
            display = FALSE
          ),
          content = list(
            type = type,
            random = TRUE
          )
        )
      )
    }
  )
}