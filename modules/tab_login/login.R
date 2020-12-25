login_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 6,
      bs4Dash::box(
        width = NULL,
        status = "primary",
        title = "Login",
        solidHeader = TRUE,
        shiny::uiOutput(
          outputId = ns("login")
        )
      )
    ),
    shiny::column(
      width = 6,
      login_user_info_ui(
        id = ns("login_user_info")
      )
    )
  )
}

login_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$login <- shiny::renderUI({
        if (.values$user$status() == "not_logged") {
          login_r()
        } else {
          logout_r()
        }
      })

      login_r <- shiny::reactive({
        htmltools::tagList(
          shiny::selectInput(
            inputId = ns("user_name"),
            label = "Username",
            choices = user_name_choices_r()
          ),
          shiny::passwordInput(
            inputId = ns("user_password"),
            label = "Password",
            placeholder = "1234"
          ),
          shiny::actionButton(
            inputId = ns("user_login"),
            label = "Log in",
            width = "100%"
          )
        )
      })

      shiny::observeEvent(input$user_login, {
        user_pwd <- db_get_password(
          db = .values$db,
          name = input$user_name
        )

        pwd_correct <- bcrypt::checkpw(input$user_password, user_pwd)

        if (pwd_correct) {
          .values$user$status(db_get_user_status(.values$db, input$user_name))
          .values$user$name(input$user_name)
          .values$user$last_logged(db_get_user_last_logged(.values$db, input$user_name))
          db_log_user_in(.values$db, input$user_name)
          .values$update$user(.values$update$user() + 1)

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

      logout_r <- shiny::reactive({
        shiny::actionButton(
          inputId = ns("user_logout"),
          label = "Log out",
          width = "100%"
        )
      })

      shiny::observeEvent(input$user_logout, {
        .values$user$status("not_logged")
        .values$user$name("")
        .values$user$last_logged("")

        shiny::showNotification(
          ui = "Logout successful. See you again.",
          type = "default",
          duration = 3
        )
      })

      user_name_choices_r <- shiny::reactive({
        .values$update$user()

        sort(db_get_user_names(.values$db))
      })



      login_user_info_server(
        id = "login_user_info",
        .values = .values
      )
    }
  )
}
