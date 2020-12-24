container_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboardPlus::dashboardPage(
    header = shinydashboardPlus::dashboardHeader(
      title = "myPaintings",
      shinydashboardPlus::userOutput(
        id = ns("user")
      )
    ),
    sidebar = shinydashboardPlus::dashboardSidebar(
      # sidebar_menu_ui(
      #   id = ns("sidebar_menu")
      # ),
      shinydashboard::sidebarMenuOutput(ns("menu")),
      #shinydashboard::sidebarMenu(shinydashboard::menuItem(text = "Menu Item")),
      collapsed = FALSE
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
    )
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
          name = "Divad Nojnarg", 
          image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg", 
          title = "shinydashboardPlus",
          subtitle = "Author", 
          footer = p("The footer", class = "text-center"),
          fluidRow(
            dashboardUserItem(
              width = 6,
              socialButton(
                href = "https://dropbox.com",
                icon = icon("dropbox")
              )
            ),
            dashboardUserItem(
              width = 6,
              socialButton(
                href = "https://github.com",
                icon = icon("github")
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
        id = "sidebar_menu",
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
