account_info_ui <- function(id) {
  ns <- shiny::NS(id)
  
  htmltools::tags$li(
    class = "nav-item dropdown",
    htmltools::tags$a(
      href = "#",
      class = "nav-link",
      shiny::uiOutput(
        outputId = ns("account")
      )
    )
  )
}

account_info_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      account_label_r <- shiny::reactive({
        .values$settings$dollar_format(.values$user$account())
      })
      
      output$account <- bs4Dash::renderMenu({
        if (.values$user$status() != "not_logged") {
          shiny::actionLink(
            inputId = ns("open_account_tab"),
            label = account_label_r(),
            icon = shiny::icon("money-bill-alt")
          )
        }
      })
      
      shiny::observeEvent(input$open_account_tab, {
        .values$update_sidebar("wallet")
      })
    }
  )
}