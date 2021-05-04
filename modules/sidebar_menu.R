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
        not_logged = c("welcome", "getting_started"),
        admin = c(
          "browse", "database", "user_management", "getting_started"
        ),
        user = c(
          "collection", "browse", "buy", "wallet", "getting_started"
        )
      )

      # List of all possible menu items. Extraction is done according to access
      # right
      menu_item_list <- list(
        welcome = bs4Dash::menuItem(
          text = "Welcome",
          tabName = "welcome",
          icon = shiny::icon("certificate")
        ),
        getting_started = bs4Dash::menuItem(
          text = "Getting Started",
          tabName = "getting_started",
          icon = shiny::icon("rocket")
        ),
        browse = bs4Dash::menuItem(
          text = "Browse",
          tabName = "browse",
          icon = shiny::icon("images")
        ),
        collection = bs4Dash::menuItem(
          text = "Collection",
          tabName = "collection",
          icon = shiny::icon("portrait")
        ),
        buy = bs4Dash::menuItem(
          text = "Buy",
          tabName = "buy",
          icon = shiny::icon("shopping-bag")
        ),
        wallet = bs4Dash::menuItem(
          text = "Wallet",
          tabName = "wallet",
          icon = shiny::icon("wallet")
        ),
        database = bs4Dash::menuItem(
          text = "Database",
          tabName = "database",
          icon = shiny::icon("database")
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
        menu_items <- unname(menu_item_list[access_list[[.values$user_rvs$status]]])

        args <- list(
          id = ns("sidebar")
        )
        
        do.call(sidebarMenu, c(menu_items, args))
      })
      
      
      
      return_list <- list(
        sidebar_r = shiny::reactive(input$sidebar)
      )
      
      return(return_list)
    }
  )
}