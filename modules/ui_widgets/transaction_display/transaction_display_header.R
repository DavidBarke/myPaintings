transaction_display_header_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    bs4Dash::box(
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::dateRangeInput(
            inputId = ns("date"),
            label = NULL,
            start = "2019-01-01",
            end = "2021-12-31",
            min = "2019-01-01",
            max = "2021-12-31",
            width = "100%"
          )
        ),
        shiny::column(
          width = 6,
          shiny::actionButton(
            inputId = ns("apply_date"),
            label = "Apply",
            width = "100%"
          )
        ),
        shiny::column(
          width = 6,
          shiny::actionButton(
            inputId = ns("reset_date"),
            label = "Reset",
            width = "100%"
          )
        )
      )
    )
  )
}

transaction_display_header_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      default_date_start <- "2019-01-01"
      default_date_end <- "2021-12-31"
      
      date_start_rv <- shiny::reactiveVal(default_date_start)
      date_end_rv <- shiny::reactiveVal(default_date_end)
      
      shiny::observeEvent(input$apply_date, {
        date_start_rv(as.character(input$date[1]))
        date_end_rv(as.character(input$date[2]))
      })
      
      shiny::observeEvent(input$reset_date, {
        date_start_rv(default_date_start)
        date_end_rv(default_date_end)
        shiny::updateDateRangeInput(
          inputId = "date",
          start = default_date_start,
          end = default_date_end
        )
      })
      
      transaction_ids_r <- shiny::reactive({
        db_get_transaction_ids_by_filter(
          db = .values$db,
          user_id = .values$user_rv()$user_id,
          date_start = date_start_rv(),
          date_end = date_end_rv()
        )
      })
      
      transactions_r <- shiny::reactive({
        db_get_transaction_display(
          .values$db,
          transaction_ids_r()
        )
      })
      
      return_list <- list(
        date_start_r = shiny::reactive(date_start_rv()),
        date_end_r = shiny::reactive(date_end_rv()),
        n_r = shiny::reactive(nrow(transactions_r())),
        transactions_r = transactions_r
      )
      
      return(return_list)
    }
  )
}