login_ui <- function(id) {
  ns <- shiny::NS(id)
  
  dropdown_menu_output(
    outputId = ns("login")
  )
}

login_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      
      output$login <- shiny::renderUI({
        if (.values$user_rv()$status == "not_logged") {
          dropdown_menu(
            title = "Login",
            dropdown_menu_item(
              item = shiny::uiOutput(
                outputId = ns("user_name")
              ),
              width = 12
            ),
            dropdown_menu_item(
              item = shiny::uiOutput(
                outputId = ns("user_password")
              ),
              width = 12
            ),
            dropdown_menu_item(
              item = shiny::uiOutput(
                outputId = ns("user_login")
              ),
              width = 12
            )
          )
        }
      })
      
      output$user_name <- shiny::renderUI({
        shiny::textInput(
          inputId = ns("user_name"),
          label = "Username"
        )
      })
      
      output$user_password <- shiny::renderUI({
        shiny::passwordInput(
          inputId = ns("user_password"),
          label = "Password",
          placeholder = "1234"
        )
      })

      output$user_login <- shiny::renderUI({
        shiny::actionButton(
          inputId = ns("user_login"),
          label = "Log in",
          width = "100%"
        )
      })

      shiny::observeEvent(input$user_login, {
        if (!db_has_user_name(.values$db, input$user_name)) {
          bs4Dash::toast(
            title = "Username does not exist. Please try again.",
            options = .values$settings$toast(
              class = "bg-danger",
              delay = 3000
            )
          )
          return()
        }
        
        user_pwd <- db_get_password(
          db = .values$db,
          name = input$user_name
        )

        pwd_correct <- bcrypt::checkpw(input$user_password, user_pwd)

        if (pwd_correct) {
          user_id <- db_get_user_id(db, input$user_name)
          entry <- db_get_user_entry(.values$db, user_id)
          .values$user_rv(entry)
          db_log_user_in(.values$db, input$user_name)
          .values$update$db_user_rv(.values$update$db_user_rv() + 1)

          shiny::showNotification(
            ui = "Login successful.",
            type = "default",
            duration = 3
          )
        } else {
          shiny::showNotification(
            ui = "Wrong password! Please try again.",
            type = "error",
            duration = 3
          )
        }

        shiny::updateTextInput(
          session = session,
          inputId = "user_password",
          value = ""
        )
      })
    }
  )
}
