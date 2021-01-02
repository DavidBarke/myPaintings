filter_table_condition_ui <- function(id) {
  ns <- shiny::NS(id)
  
  choices <- c(
    "Owner" = "name",
    "Painter" = "painter",
    "Title" = "title",
    "School" = "school"
  )
  
  htmltools::tags$tr(
    class = "filter-table-row",
    htmltools::tags$td(
      style = "width: 30%",
      shiny::selectInput(
        inputId = ns("filter_by"),
        label = NULL,
        choices = choices,
        selected = "title"
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

filter_table_condition_server <- function(
  id, .values, query_text_start_r, query_text_in_r, query_params_in_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      update_title_choices_rv <- shiny::reactiveVal(0)
      
      output$value <- shiny::renderUI({
        value_dict[[input$filter_by]]
      })
      
      value_dict <- list(
        name = shiny::uiOutput(
          outputId = ns("value_name")
        ),
        painter = shiny::uiOutput(
          outputId = ns("value_painter")
        ),
        title = shiny::uiOutput(
          outputId = ns("value_title")
        ),
        school = shiny::uiOutput(
          outputId = ns("value_school")
        )
      )
      
      ## By user name ----
      output$value_name <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("value_name"),
          label = NULL,
          choices = db_get_user_ids(.values$db)
        )
      })
      
      ## By painter ----
      output$value_painter <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("value_painter"),
          label = NULL,
          choices = db_get_painters(.values$db)
        )
      })
      
      ## By title ----
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
      
      ## By school ----
      output$value_school <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("value_school"),
          label = NULL,
          choices = db_get_schools(.values$db)
        )
      })
      
      query_text_out_r <- shiny::reactive({
        c(
          query_text_in_r(),
          query_text_dict[[shiny::req(input$filter_by)]]
        )
      })
      
      query_text_dict <- list(
        name = "user_image.user_id = ?",
        painter = "image.painter_id = ?",
        title = "image.rowid = ?",
        school = "image.school = ?"
      )
      
      query_params_out_r <- shiny::reactive({
        c(
          query_params_in_r(),
          query_params_dict_fun[[shiny::req(input$filter_by)]]()
        )
      })
      
      # Reactive that returns a one element list containing the params
      # which correspond to the query text
      query_params_dict_fun <- list(
        name = shiny::reactive(shiny::req(input$value_name)),
        painter = shiny::reactive(shiny::req(input$value_painter)),
        title = shiny::reactive(shiny::req(input$value_title)),
        school = shiny::reactive(shiny::req(input$value_school))
      )
      
      return_list <- list(
        query_text_out_r = query_text_out_r,
        query_params_out_r = query_params_out_r
      )
      
      return(return_list)
    }
  )
}