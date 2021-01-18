container_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::bs4DashPage(
    header = bs4Dash::bs4DashNavbar(
      title = "myPaintings",
      rightUi = htmltools::tagList(
        account_info_ui(
          id = ns("account_info")
        ),
        user_info_ui(
          id = ns("user_info")
        ),
        login_ui(
          id = ns("login")
        )
      )
    ),
    sidebar = bs4Dash::bs4DashSidebar(
      id = ns("sidebar"),
      sidebar_menu_ui(
        id = ns("sidebar_menu")
      )
    ),
    body = bs4Dash::bs4DashBody(
      bs4Dash::bs4TabItems(
        bs4Dash::bs4TabItem(
          tabName = "welcome",
          welcome_ui(
            id = ns("welcome")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "browse",
          browse_ui(
            id = ns("browse")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "collection",
          collection_ui(
            id = ns("collection")
          )
        ),
        bs4Dash::tabItem(
          tabName = "buy",
          buy_ui(
            id = ns("buy")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "wallet",
          wallet_ui(
            id = ns("wallet")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "database",
          database_ui(
            id = ns("database")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "user_management",
          user_management_ui(
            id = ns("user_management")
          )
        ),
        bs4Dash::bs4TabItem(
          tabName = "settings",
          settings_ui(
            id = ns("settings")
          )
        )
      )
    ),
    title = "myPaintings"
  )
}

container_server <- function(id, .values) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      # Register function for updating sidebar from other modules
      .values$update_sidebar <- function(tabName) {
        shinydashboard::updateTabItems(
          session = .values$sidebar$session,
          inputId = .values$sidebar$id,
          selected = tabName
        )
      }
      
      sidebar_menu_server(
        id = "sidebar_menu", 
        .values = .values
      )
      
      ## Navbar ----
      account_info_server(
        id = "account_info",
        .values = .values
      )
      
      user_info_server(
        id = "user_info",
        .values = .values
      )
      
      login_server(
        id = "login",
        .values = .values
      )

      ## Tab Items ----
      welcome_server(
        id = "welcome",
        .values = .values
      )
      
      browse_server(
        id = "browse",
        .values = .values
      )
      
      collection_server(
        id = "collection",
        .values = .values
      )
      
      buy_server(
        id = "buy",
        .values = .values
      )
      
      wallet_server(
        id = "wallet",
        .values = .values
      )
      
      database_server(
        id = "database",
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