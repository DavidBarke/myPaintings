filter_table_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tagList(
    htmltools::tags$table(
      id = ns("table"),
      class = "filter-table"
    ),
    shiny::actionButton(
      inputId = ns("add_condition"),
      label = NULL,
      icon = shiny::icon("plus"),
      width = "100%"
    ),
    shiny::actionButton(
      inputId = ns("reset"),
      label = "Reset",
      width = "100%"
    ),
    shiny::actionButton(
      inputId = ns("apply"),
      label = "Apply",
      width = "100%"
    )
  )
}

filter_table_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # Number of currently active conditions
      n_conditions_rv <- shiny::reactiveVal(0)
      # Maximum number of conditions that has been reached during particular session
      max_conditions_rv <- shiny::reactiveVal(0)
      # Triggered when n_conditions_rv is set to 1
      first_condition_rv <- shiny::reactiveVal(0)
      
      return <- environment()
      # List of server returns
      return$row <- list()
      
      shiny::observeEvent(input$add_condition, {
        n_conditions_rv(n_conditions_rv() + 1)
        
        if (n_conditions_rv() == 1) first_condition_rv(first_condition_rv() + 1)
        
        if (n_conditions_rv() > max_conditions_rv()) {
          query_text_in_r <- if (n_conditions_rv() > 1) {
            return$row[[n_conditions_rv() - 1]]$query_text_out_r
          } else {
            shiny::reactive(character())
          }
          
          query_params_in_r <- if (n_conditions_rv() > 1) {
            return$row[[n_conditions_rv() - 1]]$query_params_out_r
          } else {
            shiny::reactive(list())
          }
          
          return$row[[n_conditions_rv()]] <- filter_table_condition_server(
            id = "filter_table_condition" %_% n_conditions_rv(),
            .values = .values,
            query_text_start_r = query_text_start_r,
            query_text_in_r = query_text_in_r,
            query_params_in_r = query_params_in_r,
            first_condition_r = shiny::reactive(first_condition_rv())
          )
          
          max_conditions_rv(n_conditions_rv())
        }
        
        shiny::insertUI(
          selector = paste0("#", ns("table")),
          where = "beforeEnd",
          ui = filter_table_condition_ui(
            id = ns("filter_table_condition" %_% n_conditions_rv())
          )
        )
      })
      
      shiny::observeEvent(input$reset, {
        n_conditions_rv(0)
        
        shiny::removeUI(
          selector = paste0("#", ns("table"), " .filter-table-row"),
          multiple = TRUE
        )
      })
      
      query_text_start_r <- shiny::reactive({
        "SELECT image.rowid AS image_id FROM image INNER JOIN user_image ON image.rowid = user_image.image_id"
      })
      
      query_text_result_r <- shiny::reactive({
        if (n_conditions_rv() == 0) return(query_text_start_r())
        
        construct_query_text(
          query_text_start_r(),
          return$row[[n_conditions_rv()]]$query_text_out_r()
        )
      })
      
      query_params_result_r <- shiny::reactive({
        if (n_conditions_rv() == 0) return(list())
        
        return$row[[n_conditions_rv()]]$query_params_out_r()
      })
      
      # shiny::observeEvent(query_params_result_r(), {
      #   cat(query_text_result_r(), "\n")
      #   str(query_params_result_r())
      # })
      
      image_ids_r <- shiny::eventReactive(input$apply, {
        if (length(query_params_result_r()) == 0) {
          DBI::dbGetQuery(
            .values$db,
            query_text_result_r()
          )
        } else {
          DBI::dbGetQuery(
            .values$db,
            query_text_result_r(),
            params = query_params_result_r()
          )
        }
      })
      
      # shiny::observeEvent(image_ids_r(), {
      #   print(dplyr::as_tibble(image_ids_r()))
      # })
      
      return_list <- list(
        image_ids_r = shiny::reactive(image_ids_r()$image_id)
      )
      
      return(return_list)
    }
  )
}