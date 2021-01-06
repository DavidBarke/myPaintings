filter_table_condition_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    id = ns("row"),
    class = "filter-table-row",
    shiny::column(
      width = 4,
      shiny::uiOutput(
        outputId = ns("filter_by")
      )
    ),
    shiny::column(
      width = 2,
      shiny::uiOutput(
        outputId = ns("operation")
      )
    ),
    shiny::column(
      width = 4,
      shiny::uiOutput(
        outputId = ns("value")
      )
    ),
    shiny::column(
      width = 2,
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
  n_conditions_r,
  tab
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
      
      # did this condition change from not active to active
      gets_active_r <- shiny::reactive({
        gets_active_rv()
      })
      
      gets_active_rv <- shiny::reactiveVal(0)
      
      last_n_conditions_rv <- shiny::reactiveVal(0)
      
      shiny::observeEvent(n_conditions_r(), {
        n <- n_conditions_r()
        last_n <- last_n_conditions_rv()
        
        # Update early (before first return)
        last_n_conditions_rv(n)
        
        is_active <- n == index
        
        if (is_active == is_active_rv()) return()
      
        # only update is_active_rv when active status changed
        is_active_rv(is_active)
        
        # only update gets_active_rv when active status changes to active and
        # condition was just added (not removed)
        if (is_active && n > last_n) {
          gets_active_rv(gets_active_rv() + 1)
        }
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
      
      ## Filter by ----
      tab_choices <- list(
        browse = c("title", "painter", "name", "school", "type", "status", "price"),
        collection = c("title", "painter", "school", "type", "status"),
        trade = c("title", "painter", "name", "school", "type", "price")
      )
      
      choices <- c(
        "Owner" = "name",
        "Painter" = "painter",
        "Title" = "title",
        "School" = "school",
        "Type" = "type",
        "Status" = "status",
        "Price" = "price"
      )
      
      choices <- choices[match(tab_choices[[tab]], choices)]
      
      output$filter_by <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("filter_by"),
          label = NULL,
          choices = choices
        )
      })
      
      ## Operation ----
      operation_group_dict <- c(
        name = "text",
        painter = "text",
        title = "text",
        school = "text",
        type = "text",
        status = "status",
        price = "price"
      )
      
      operation_dict <- list(
        text = shiny::uiOutput(
          outputId = ns("operation_text")
        ),
        status = shiny::uiOutput(
          outputId = ns("operation_status")
        ),
        price = shiny::uiOutput(
          outputId = ns("operation_price")
        )
      )
      
      operation_r <- shiny::reactive({
        operation_group_dict[[shiny::req(input$filter_by)]]
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
      
      output$operation_price <- shiny::renderUI({
        max <- db_get_max_offered_price(.values$db)
        
        shiny::sliderInput(
          inputId = ns("operation_price"),
          label = NULL,
          min = 0,
          max = max,
          value = c(0, max),
          pre = "$"
        )
      })
      
      ## Value ----
      output$value <- shiny::renderUI({
        value_dict[[shiny::req(input$filter_by)]]
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
            selected = shiny::isolate(input$value_name),
            multiple = multiple
          )
        }
      })
      
      ## By painter ----
      update_painter_choices_rv <- shiny::reactiveVal(0)
      
      output$value_painter <- shiny::renderUI({
        input$filter_by
        gets_active_r()
        
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
          
          shiny::selectizeInput(
            inputId = ns("value_painter"),
            label = NULL,
            choices = NULL,
            selected = shiny::isolate(input$value_painter),
            multiple = multiple
          )
        }
      })
      
      shiny::observeEvent(update_painter_choices_rv(), {
        shiny::updateSelectizeInput(
          inputId = "value_painter",
          choices = painters_r(),
          selected = input$value_painter,
          server = TRUE
        )
        
        if (!is_active_r()) {
          js$disable_selectize_input(id = ns("value_title"), asis = TRUE)
        }
      })
      
      painters_r <- shiny::reactive({
        db_get_painters(.values$db)
      })
      
      ## By title ----
      update_title_choices_rv <- shiny::reactiveVal(0)
      
      output$value_title <- shiny::renderUI({
        input$filter_by
        gets_active_r()
        
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
            choices = NULL,
            selected = shiny::isolate(input$value_title),
            multiple = multiple
          )
        }
      })
      
      shiny::observeEvent(update_title_choices_rv(), {
        selected <- if (is.null(input$value_title) || input$value_title == "") {
          NULL
        } else input$value_title
        
        shiny::updateSelectizeInput(
          inputId = "value_title",
          choices = image_ids_r(),
          selected = selected,
          server = TRUE
        )
        
        if (!is_active_r()) {
          js$disable_selectize_input(id = ns("value_title"), asis = TRUE)
        }
      }, ignoreInit = TRUE)
      
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
            selected = shiny::isolate(input$value_school),
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
            selected = shiny::isolate(input$value_type),
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
          ),
          selected = shiny::isolate(input$value_status)
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
              shiny::req(input$operation_text)
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