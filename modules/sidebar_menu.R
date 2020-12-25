sidebar_menu_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::sidebarMenuOutput(
    outputId = ns("menu")
  )
}

sidebar_menu_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      # Name of menu items that are visible according to access right
      access_list <- list(
        not_logged = "login",
        admin = c(
          "login", "images", "user_management", "settings"
        ),
        user = c(
          "login", "images", "settings"
        )
      )

      # List of all possible menu items. Extraction is done according to access
      # right
      menu_item_list <- list(
        login = shinydashboard::menuItem(
          text = "Login",
          tabName = "login",
          icon = shiny::icon("sign-in-alt")
        ),
        images = shinydashboard::menuItem(
          text = "Images",
          tabName = "images",
          icon = shiny::icon("images")
        ),
        user_management = shinydashboard::menuItem(
          text = "User Management",
          tabName = "user_management",
          icon = shiny::icon("user-edit")
        ),
        settings = shinydashboard::menuItem(
          text = "Settings",
          tabName = "settings",
          icon = shiny::icon("cog")
        )
      )

      output$menu <- shinydashboard::renderMenu({
        sidebar_menu_r()
      })

      sidebar_menu_r <- shiny::reactive({
        menu_items <- unname(menu_item_list[access_list[[.values$user$status()]]])

        shinydashboard::sidebarMenu(.list = menu_items)
      })
    }
  )
}