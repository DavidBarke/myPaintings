filter_table_condition_ui <- function(id) {
  ns <- shiny::NS(id)
  
  choices <- c(
    "Owner" = "name",
    "Painter" = "painter",
    "Title" = "title",
    "School" = "school",
    "Type" = "type",
    "Status" = "status"
  )
  
  htmltools::tags$tr(
    id = ns("row"),
    class = "filter-table-row",
    htmltools::tags$td(
      shiny::selectInput(
        inputId = ns("filter_by"),
        label = NULL,
        choices = choices,
        selected = "title"
      )
    ),
    htmltools::tags$td(
      shiny::uiOutput(
        outputId = ns("operation")
      )
    ),
    htmltools::tags$td(
      shiny::uiOutput(
        outputId = ns("value")
      )
    ),
    htmltools::tags$td(
      shiny::uiOutput(
        outputId = ns("remove_btn")
      )
    )
  )
}

filter_table_condition_server <- function(
  id, .values, 
  index,
  query_text_start_r, query_text_in_r, query_params_in_r,
  first_condition_r, # needed to trigger server side selectize inputs,
  n_conditions_r
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      force(index)
      
      # is this condition the last condition
      is_active_r <- shiny::reactive({
        # keeping updates on a minimum by querying reactiveVal
        is_active_rv()
      })
      
      is_active_rv <- shiny::reactiveVal(FALSE)
      
      shiny::observeEvent(n_conditions_r(), {
        is_active <- n_conditions_r() == index
        
        if (is_active == is_active_rv()) return()
      
        # only update is_active_rv when active status changed
        is_active_rv(is_active)
      })
      
      shiny::observeEvent(is_active_r(), {
        if (!is_active_r()) {
          shinyjs::disable(ns("row"), asis = TRUE)
        } else {
          shinyjs::enable(ns("row"), asis = TRUE)
        }
      })
      
      output$remove_btn <- shiny::renderUI({
        if (is_active_r()) {
          shiny::actionButton(
            inputId = ns("remove_row"),
            label = NULL,
            icon = shiny::icon("times")
          )
        }
      })
      
      remove_rv <- shiny::reactiveVal(0)
      
      shiny::observeEvent(input$remove_row, {
        shiny::removeUI(
          selector = paste0("#", ns("row"))
        )
        
        # Notify filter table
        remove_rv(remove_rv() + 1)
      })
      
      ## Operation ----
      operation_group_dict <- c(
        name = "text",
        painter = "text",
        title = "text",
        school = "text",
        type = "text",
        status = "status"
      )
      
      operation_dict <- list(
        text = shiny::uiOutput(
          outputId = ns("operation_text")
        ),
        status = shiny::uiOutput(
          outputId = ns("operation_status")
        )
      )
      
      operation_r <- shiny::reactive({
        operation_group_dict[[input$filter_by]]
      })
      
      output$operation <- shiny::renderUI({
        operation_dict[[operation_r()]]
      })
      
      output$operation_text <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("operation_text"),
          label = NULL,
          choices = c(
            "=" = "=",
            "IN" = "IN",
            "REGEXP" = "REGEXP"
          )
        )
      })
      
      output$operation_status <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("operation_status"),
          label = NULL,
          choices = c("=" = "=")
        )
      })
      
      ## Value ----
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
        ),
        type = shiny::uiOutput(
          outputId = ns("value_type")
        ),
        status = shiny::uiOutput(
          outputId = ns("value_status")
        )
      )
      
      ## By user name ----
      output$value_name <- shiny::renderUI({
        if (shiny::req(input$operation_text) == "REGEXP") {
          shiny::textInput(
            inputId = ns("value_name"),
            label = NULL
          )
        } else {
          multiple <- shiny::req(input$operation_text) == "IN"
          
          shiny::selectInput(
            inputId = ns("value_name"),
            label = NULL,
            choices = db_get_user_ids(.values$db),
            multiple = multiple
          )
        }
      })
      
      ## By painter ----
      update_painter_choices_rv <- shiny::reactiveVal(0)
      
      output$value_painter <- shiny::renderUI({
        input$filter_by
        #first_condition_r()
        is_active_r()
        
        if (shiny::req(input$operation_text) == "REGEXP") {
          shiny::textInput(
            inputId = ns("value_painter"),
            label = NULL
          )
        } else {
          update_painter_choices_rv(
            shiny::isolate(update_painter_choices_rv()) + 1
          )
          
          multiple <- shiny::req(input$operation_text) == "IN"
          
          shiny::selectInput(
            inputId = ns("value_painter"),
            label = NULL,
            choices = NULL,
            multiple = multiple
          )
        }
      })
      
      shiny::observeEvent(update_painter_choices_rv(), {
        shiny::updateSelectizeInput(
          inputId = "value_painter",
          choices = painters_r(),
          server = TRUE
        )
      })
      
      painters_r <- shiny::reactive({
        db_get_painters(.values$db)
      })
      
      ## By title ----
      update_title_choices_rv <- shiny::reactiveVal(0)
      
      output$value_title <- shiny::renderUI({
        input$filter_by
        #first_condition_r()
        is_active_r()
        
        if (shiny::req(input$operation_text) == "REGEXP") {
          shiny::textInput(
            inputId = ns("value_title"),
            label = NULL
          )
        } else {
          update_title_choices_rv(shiny::isolate(update_title_choices_rv()) + 1)
          
          multiple <- shiny::req(input$operation_text) == "IN"
          
          shiny::selectizeInput(
            inputId = ns("value_title"),
            label = NULL,
            choices = NULL
          )
        }
      })
      
      update_title_choices_occured_rv <- shiny::reactiveVal(0)
      
      shiny::observeEvent(update_title_choices_rv(), {
        shiny::updateSelectizeInput(
          inputId = "value_title",
          choices = image_ids_r(),
          server = TRUE
        )
        
        update_title_choices_occured_rv(update_title_choices_occured_rv() + 1)
      })
      
      shiny::observeEvent(update_title_choices_occured_rv(), {
        if (!is_active_r()) {
          js$disable_selectize_input(id = ns("value_title"), asis = TRUE)
        }
      })
      
      image_ids_r <- shiny::reactive({
        db_get_image_ids(.values$db)
      })
      
      ## By school ----
      output$value_school <- shiny::renderUI({
        if (shiny::req(input$operation_text) == "REGEXP") {
          shiny::textInput(
            inputId = ns("value_school"),
            label = NULL
          )
        } else {
          multiple <- shiny::req(input$operation_text) == "IN"
          
          shiny::selectInput(
            inputId = ns("value_school"),
            label = NULL,
            choices = db_get_image_schools(.values$db),
            multiple = multiple
          )
        }
      })
      
      ## By type ----
      output$value_type <- shiny::renderUI({
        if (shiny::req(input$operation_text) == "REGEXP") {
          shiny::textInput(
            inputId = ns("value_type"),
            label = NULL
          )
        } else {
          multiple <- shiny::req(input$operation_text) == "IN"
          
          choices <- db_get_image_types(.values$db)
          names(choices) <- stringr::str_to_title(choices)
          
          shiny::selectInput(
            inputId = ns("value_type"),
            label = NULL,
            choices = choices,
            multiple = multiple
          )
        }
      })
      
      ## By status ----
      output$value_status <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("value_status"),
          label = NULL,
          choices = c(
            "All" = "all",
            "Offered" = "offered",
            "Not Offered" = "not_offered"
          )
        )
      })
      
      ## Query ----
      query_operation_dict <- list(
        text = shiny::reactive(shiny::req(input$operation_text)),
        status = shiny::reactive(shiny::req(input$operation_status))
      )
      
      query_operation_r <- shiny::reactive({
        query_operation_dict[[operation_r()]]()
      })
      
      query_text_out_r <- shiny::reactive({
        c(
          query_text_in_r(),
          paste(
            query_col_dict(
              shiny::req(input$filter_by),
              operation_r()
            ),
            query_operator_dict[[shiny::req(input$operation_text)]],
            "?"
          )
        )
      })
      
      query_col_dict_default <- list(
        name = "user_image.user_id",
        painter = "image.painter_id",
        title = "image.rowid",
        school = "image.school",
        type = "image.type",
        status = "is_offered"
      )
      
      query_col_dict_regexp <- list(
        name = "user.name",
        painter = "painter.name",
        title = "image.title",
        school = "image.school",
        type = "image.type"
        # status not needed
      )
      
      query_col_dict <- function(filter_by, operation_text) {
        if (operation_text == "REGEXP") {
          query_col_dict_regexp[[filter_by]]
        } else {
          query_col_dict_default[[filter_by]]
        }
      }
      
      query_operator_dict <- list(
        "=" = "=",
        "IN" = "=",
        "REGEXP" = "REGEXP"
      )
      
      query_params_out_r <- shiny::reactive({
        c(
          query_params_in_r(),
          list(query_params_dict_fun[[shiny::req(input$filter_by)]]())
        )
      })
      
      # Reactive that returns a one element list containing the params
      # which correspond to the query text
      query_params_dict_fun <- list(
        name = shiny::reactive(shiny::req(input$value_name)),
        painter = shiny::reactive(shiny::req(input$value_painter)),
        title = shiny::reactive(shiny::req(input$value_title)),
        school = shiny::reactive(shiny::req(input$value_school)),
        type = shiny::reactive(shiny::req(input$value_type)),
        status = shiny::reactive({
          switch(
            shiny::req(input$value_status),
            "all" = c(0, 1),
            "offered" = 1,
            "not_offered" = 0
          )
        })
      )
      
      ## Return ----
      return_list <- list(
        query_text_out_r = query_text_out_r,
        query_params_out_r = query_params_out_r,
        remove_r = shiny::reactive(remove_rv())
      )
      
      return(return_list)
    }
  )
}