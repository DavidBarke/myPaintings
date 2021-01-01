filter_table_condition_ui <- function(id) {
  ns <- shiny::NS(id)
  
  choices <- c(
    "Owner" = "name",
    "Title" = "title"
  )
  
  htmltools::tags$tr(
    class = "filter-table-row",
    htmltools::tags$td(
      style = "width: 30%",
      shiny::selectInput(
        inputId = ns("filter_by"),
        label = NULL,
        choices = choices
      )
    ),
    htmltools::tags$td(
      style = "width: 20%",
      "="
    ),
    htmltools::tags$td(
      style = "width: 50%",
      shiny::uiOutput(
        outputId = ns("value")
      )
    )
  )
}

filter_table_condition_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      update_title_choices_rv <- shiny::reactiveVal(0)
      
      output$value <- shiny::renderUI({
        value_dict[[print(input$filter_by)]]
      })
      
      value_dict <- list(
        name = shiny::uiOutput(
          outputId = ns("value_name")
        ),
        title = shiny::uiOutput(
          outputId = ns("value_title")
        )
      )
      
      output$value_name <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("value_name"),
          label = NULL,
          choices = db_get_user_ids(.values$db)
        )
      })
      
      output$value_title <- shiny::renderUI({
        update_title_choices_rv(shiny::isolate(update_title_choices_rv()) + 1)
        
        shiny::selectizeInput(
          inputId = ns("value_title"),
          label = NULL,
          choices = NULL
        )
      })
      
      shiny::observeEvent(update_title_choices_rv(), {
        shiny::updateSelectizeInput(
          inputId = "value_title",
          choices = image_ids_r(),
          server = TRUE
        )
      })
      
      image_ids_r <- shiny::reactive({
        db_get_image_ids(.values$db)
      })
    }
  )
}