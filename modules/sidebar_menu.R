sidebar_menu_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::sidebarMenuOutput(
    outputId = ns("menu")
  )
}

sidebar_menu_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      
      .values$sidebar <- list(
        id = "sidebar",
        session = session
      )

      # Name of menu items that are visible according to access right
      access_list <- list(
        not_logged = "login",
        admin = c(
          "login", "images", "wallet", "user_management", "settings"
        ),
        user = c(
          "login", "images", "wallet", "settings"
        )
      )

      # List of all possible menu items. Extraction is done according to access
      # right
      menu_item_list <- list(
        login = bs4Dash::menuItem(
          text = "Login",
          tabName = "login",
          icon = shiny::icon("sign-in-alt")
        ),
        images = bs4Dash::menuItem(
          text = "Images",
          tabName = "images",
          icon = shiny::icon("images")
        ),
        wallet = bs4Dash::menuItem(
          text = "Wallet",
          tabName = "wallet",
          icon = shiny::icon("wallet")
        ),
        user_management = bs4Dash::menuItem(
          text = "User Management",
          tabName = "user_management",
          icon = shiny::icon("user-edit")
        ),
        settings = bs4Dash::menuItem(
          text = "Settings",
          tabName = "settings",
          icon = shiny::icon("cog")
        )
      )

      output$menu <- bs4Dash::renderMenu({
        sidebar_menu_r()
      })

      sidebar_menu_r <- shiny::reactive({
        menu_items <- unname(menu_item_list[access_list[[.values$user$status()]]])

        args <- list(
          id = ns(.values$sidebar$id)
        )
        
        do.call(sidebarMenu, c(menu_items, args))
      })
    }
  )
}