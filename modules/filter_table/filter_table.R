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
    )
  )
}

filter_table_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      n_conditions_rv <- shiny::reactiveVal(0)
      max_conditions_rv <- shiny::reactiveVal(0)
      
      return <- environment()
      return$row <- list()
      
      shiny::observeEvent(input$add_condition, {
        n_conditions_rv(n_conditions_rv() + 1)
        
        if (n_conditions_rv() > max_conditions_rv()) {
          query_text_in_r <- if (n_conditions_rv() > 1) {
            return$row[[n_conditions_rv() - 1]]$query_text_out_r
          } else {
            shiny::reactive(character())
          }
          
          query_params_in_r <- if (n_conditions_rv() > 1) {
            return$row[[n_conditions_rv() - 1]]$query_params_out_r
          } else {
            list()
          }
          
          return$row[[n_conditions_rv()]] <- filter_table_condition_server(
            id = "filter_table_condition" %_% n_conditions_rv(),
            .values = .values,
            query_text_start_r = query_text_start_r,
            query_text_in_r = query_text_in_r,
            query_params_in_r = query_params_in_r
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
        "SELECT image.rowid AS image_id FROM image INNER JOIN user_image ON image.rowid = user_image.imageid"
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
      
      shiny::observeEvent(query_params_result_r(), {
        cat(query_text_result_r(), "\n")
        str(query_params_result_r())
      })
    }
  )
}