container_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboardPlus::dashboardPagePlus(
    header = shinydashboardPlus::dashboardHeaderPlus(
      shinydashboardPlus::userOutput(
        id = ns("user")
      )
    ),
    sidebar = shinydashboard::dashboardSidebar(
      sidebar_menu_ui(
        id = ns("sidebar")
      )
    ),
    body = shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "login",
          login_ui(
            id = ns("login")
          )
        ),
        shinydashboard::tabItem(
          tabName = "images",
          images_ui(
            id = ns("images")
          )
        ),
        shinydashboard::tabItem(
          tabName = "user_management",
          user_management_ui(
            id = ns("user_management")
          )
        ),
        shinydashboard::tabItem(
          tabName = "settings",
          settings_ui(
            id = ns("settings")
          )
        )
      )
    ),
    title = "DashboardPage"
  )
}

container_server <- function(id, .values) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$menu <- shinydashboard::renderMenu({
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Menu")
        )
      })

      output$user <- renderUser({
        dashboardUser(
          name = "Mona Lisa",
          src = "img/mona_lisa.png",
          title = NULL,
          subtitle = "User",
          fluidRow(
            dashboardUserItem(
              width = 6,
              socialButton(
                url = "https://dropbox.com",
                type = "dropbox"
              )
            ),
            dashboardUserItem(
              width = 6,
              socialButton(
                url = "https://github.com",
                type = "github"
              )
            )
          )
        )
      })

      # Register function for updating sidebar from other modules
      .values$update_sidebar <- function(tabName) {
        shinydashboard::updateTabItems(
          session = session,
          inputId = "sidebar",
          selected = tabName
        )
      }

      sidebar_menu_server(
        id = "sidebar",
        .values = .values
      )

      login_server(
        id = "login",
        .values = .values
      )

      images_server(
        id = "images",
        .values = .values
      )

      user_management_server(
        id = "user_management",
        .values = .values
      )

      settings_server(
        id = "settings",
        .values = .values
      )
    }
  )
}