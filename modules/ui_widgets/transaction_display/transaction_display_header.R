transaction_display_header_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 12,
      bs4Dash::box(
        width = NULL,
        title = "myPaintings",
        status = "primary",
        solidHeader = TRUE
      )
    )
  )
}

transaction_display_header_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      transactions_r <- shiny::reactive({
        db_get_transactions_by_user_id_for_display(
          .values$db,
          .values$user_rv()$user_id
        )
      })
      
      return_list <- list(
        n_r = shiny::reactive(1),
        transactions_r = transactions_r
      )
      
      return(return_list)
    }
  )
}