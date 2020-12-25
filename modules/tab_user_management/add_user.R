add_user_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::box(
    width = NULL,
    status = "primary",
    title = "Add user",
    solidHeader = TRUE,
    collapsible = TRUE,
    shiny::textInput(
      inputId = ns("user_name"),
      label = "Username",
      placeholder = "John Doe"
    ),
    shiny::uiOutput(
      outputId = ns("wrong_name_length")
    ),
    shiny::uiOutput(
      outputId = ns("user_name_taken")
    ),
    shiny::passwordInput(
      inputId = ns("user_password_1"),
      label = "Password"
    ),
    shiny::uiOutput(
      outputId = ns("wrong_password_length")
    ),
    shiny::passwordInput(
      inputId = ns("user_password_2"),
      label = "Repeat Password"
    ),
    shiny::uiOutput(
      outputId = ns("non_matching_passwords")
    ),
    shiny::selectInput(
      inputId = ns("user_status"),
      label = "Status",
      choices = c(Administrator = "admin", User = "user")
    ),
    shiny::uiOutput(
      outputId = ns("add_user")
    )
  )
}

add_user_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$wrong_name_length <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !user_name_too_short_r(),
            paste(
              "The username needs at least",
              as_english(.values$settings$user_name$length$min),
              "characters!\n\n"
            )
          ),
          shiny::need(
            !user_name_too_long_r(),
            paste(
              "The username must not contain more than",
              as_english(.values$settings$user_name$length$max),
              "characters!\n\n"
            )
          ),
          errorClass = "PFA"
        )
      })

      output$user_name_taken <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !user_name_taken_r(),
            "Username already exists!\n\n"
          ),
          errorClass = "PFA"
        )
      })

      output$wrong_password_length <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !password_too_short_r(),
            paste(
              "The password needs at least",
              as_english(.values$settings$password$length$min),
              "characters!\n\n"
            )
          ),
          shiny::need(
            !password_too_long_r(),
            paste(
              "The password must not contain more than",
              as_english(.values$settings$user_name$length$max),
              "characters!\n\n"
            )
          ),
          errorClass = "PFA"
        )
      })

      output$non_matching_passwords <- shiny::renderUI({
        shiny::validate(
          shiny::need(
            !non_matching_passwords_r(),
            "The passwords don't match!\n\n"
          ),
          errorClass = "PFA"
        )
      })

      output$add_user <- shiny::renderUI({
        if (error_r()) {
          shinyjs::disabled(
            shiny::actionButton(
              inputId = ns("add_user"),
              label = "Add user",
              width = "100%"
            )
          )
        } else {
          shiny::actionButton(
            inputId = ns("add_user"),
            label = "Add user",
            width = "100%"
          )
        }
      })

      error_r <- shiny::reactive({
        user_name_too_short_r() ||
          user_name_too_long_r() ||
          user_name_taken_r() ||
          password_too_short_r() ||
          password_too_long_r() ||
          non_matching_passwords_r()
      })

      user_name_too_short_r <- shiny::reactive({
        nchar(input$user_name) < .values$settings$user_name$length$min
      })

      user_name_too_long_r <- shiny::reactive({
        nchar(input$user_name) > .values$settings$user_name$length$max
      })

      user_name_taken_r <- shiny::reactive({
        db_has_user_name(.values$db, input$user_name)
      })

      password_too_short_r <- shiny::reactive({
        nchar(input$user_password_1) < .values$settings$password$length$min
      })

      password_too_long_r <- shiny::reactive({
        nchar(input$user_password_1) > .values$settings$password$length$max
      })

      non_matching_passwords_r <- shiny::reactive({
        input$user_password_1 != input$user_password_2
      })

      shiny::observeEvent(input$add_user, {
        shiny::updateTextInput(
          session = session,
          inputId = "user_password_1",
          value = ""
        )

        shiny::updateTextInput(
          session = session,
          inputId = "user_password_2",
          value = ""
        )

        shiny::updateTextInput(
          session = session,
          inputId = "user_name",
          value = ""
        )

        shiny::showNotification(
          ui = paste0(
            "The user \"",
            input$user_name,
            "\" was successfully added."
          )
        )

        db_add_user(
          db = .values$db,
          name = input$user_name,
          status = input$user_status,
          password = bcrypt::hashpw(input$user_password_1),
          added_from = .values$user$name()
        )

        .values$update$user(.values$update$user() + 1)
      })
    }
  )
}
