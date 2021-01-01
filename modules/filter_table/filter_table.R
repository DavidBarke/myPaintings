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
      
      shiny::observeEvent(input$add_condition, {
        n_conditions_rv(n_conditions_rv() + 1)
        
        if (n_conditions_rv() > max_conditions_rv()) {
          filter_table_condition_server(
            id = "filter_table_condition" %_% n_conditions_rv(),
            .values = .values
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
    }
  )
}