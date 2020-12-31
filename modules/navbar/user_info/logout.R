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
        if (.values$user_rv()$status != "not_logged") {
          shiny::actionButton(
            inputId = ns("logout"),
            label = "Logout"
          )
        } 
      })
      
      shiny::observeEvent(input$logout, {
        entry <- list(
          status = "not_logged",
          name = "",
          last_logged = ""
        )
        .values$user_rv(entry)
        
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