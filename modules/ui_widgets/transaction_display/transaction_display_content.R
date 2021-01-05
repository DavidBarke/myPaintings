transaction_display_content_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    bs4Dash::box(
      title = shiny::uiOutput(outputId = ns("title")),
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      DT::dataTableOutput(
        outputId = ns("transaction_table")
      )
    )
  )
}

transaction_display_content_server <- function(id, .values, options) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$title <- shiny::renderUI({
        paste(
          "Transactions (from", 
          options$date_start_r(), 
          "until", 
          options$date_end_r(),
          ")"
        )
      })
      
      output$transaction_table <- DT::renderDataTable({
        tbl <- options$transactions_r()
        
        tbl$is_sold <- tbl$Seller == .values$user_rv()$name
        
        DT::datatable(
          tbl,
          options = list(
            columnDefs = list(
              list(
                visible = FALSE,
                targets = length(tbl)
              )
            )
          )
        ) %>%
          DT::formatCurrency(
            columns = "Price"
          ) %>%
          DT::formatStyle(
            columns = "Price",
            valueColumns = "is_sold",
            color = DT::styleEqual(
              levels = c(TRUE, FALSE),
              values = c("green", "red")
            )
          )
      })
    }
  )
}