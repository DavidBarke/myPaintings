user_info_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bs4Dash::userOutput(
    id = ns("user")
  )
}

user_info_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      name_r <- shiny::reactive({
        if (.values$user$status() == "not_logged") {
          "Login"
        } else {
          .values$user$name()
        }
      })
      
      title_r <- shiny::reactive({
        if (.values$user$status() != "not_logged") {
          .values$user$name()
        }
      })
      
      subtitle_r <- shiny::reactive({
        if (.values$user$status() != "not_logged") {
          .values$user$status()
        }
      })
      
      output$user <- bs4Dash::renderUser({
        if (.values$user$status() != "not_logged") {
          bs4Dash::dashboardUser(
            name = name_r(),
            image = "./img/empty_profile.png",
            title = title_r(),
            subtitle = subtitle_r(),
            logout_ui(
              id = ns("logout")
            )
          )
        }
      })
      
      logout_server(
        id = "logout",
        .values = .values
      )
    }
  )
}