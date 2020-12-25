logout_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bs4Dash::dashboardUserItem(
    item = shiny::uiOutput(
      outputId = ns("logout")
    ),
    width = 12
  )
}

logout_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      output$logout <- shiny::renderUI({
        if (.values$user$status() != "not_logged") {
          shiny::actionButton(
            inputId = ns("logout"),
            label = "Logout"
          )
        } 
      })
      
      shiny::observeEvent(input$logout, {
        .values$user$status("not_logged")
        .values$user$name("")
        .values$user$last_logged("")
        
        bs4Dash::toast(
          title = "Logout successful. See you again.",
          options = list(
            class = "bg-success",
            autohide = TRUE,
            position = "bottomRight"
          )
        )
      })
    }
  )
}