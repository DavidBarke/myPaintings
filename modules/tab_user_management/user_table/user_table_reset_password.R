user_table_reset_password_ui <- function(id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("reset_password"),
      label = NULL,
      icon = shiny::icon("eraser"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", this.id + Math.random())',
        inputId = ns("reset_password")
      )
    )
  )
}

user_table_reset_password_server <- function(id, .values, user_name) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$reset_password, {
        shiny::showModal(shiny::modalDialog(
          title = "Reset password",
          easyClose = TRUE,
          htmltools::div(
            paste0(
              "Are you sure, that you want to reset the password for user \"",
              user_name,
              "\"?"
            )
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_reset"),
            label = "Yes"
          )
        ))
      })

      shiny::observeEvent(input$confirm_reset, {
        shiny::removeModal()

        reset_pwd <- "1234"

        bs4Dash::toast(
          title = paste0(
            "The password for user \"",
            user_name,
            "\" was reset successfully to \"",
            reset_pwd,
            "\"."
          ),
          options = .values$settings$toast(
            class = "bg-success"
          )
        )

        db_set_password(.values$db, user_name, bcrypt::hashpw(reset_pwd))
      })
    }
  )
}
