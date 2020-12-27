database_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    bs4Dash::bs4Card(
      title = "Database",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      collapsible = FALSE,
      maximizable = FALSE,
      shiny::uiOutput(
        outputId = ns("select_table")
      ),
      DT::dataTableOutput(
        outputId = ns("table")
      )
    )
  )
}

database_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$select_table <- shiny::renderUI({
        shiny::selectInput(
          inputId = ns("select_table"),
          label = "Table",
          choices = DBI::dbListTables(.values$db)
        )
      })
      
      output$table <- DT::renderDataTable({
        shiny::req(input$select_table)
        DT::datatable(
          db_get_table(.values$db, input$select_table)
        )
      })
    }
  )
}