user_table_remove_user_ui <- function(id) {
  ns <- shiny::NS(id)

  as.character(
    shiny::actionButton(
      inputId = ns("remove"),
      label = NULL,
      icon = shiny::icon("user-minus"),
      class = "primary",
      onclick = glue::glue(
        'Shiny.setInputValue(\"{inputId}\", this.id + Math.random())',
        inputId = ns("remove")
      )
    )
  )
}

user_table_remove_user_server <- function(id,
                                            .values,
                                            user_name,
                                            status,
                                            added_from
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      shiny::observeEvent(input$remove, {
        # Check that admins can't remove themselves
        if (user_name == .values$user_rv()$name) {
          shiny::showModal(shiny::modalDialog(
            easyClose = TRUE,
            title = "Access denied!",
            htmltools::div(
              "Administrators can't remove themselves."
            ),
            footer = shiny::modalButton(
              label = NULL,
              icon = shiny::icon("window-close")
            )
          ))

          return()
        }

        shiny::showModal(shiny::modalDialog(
          easyClose = TRUE,
          title = "Delete user",
          htmltools::div(
            paste0(
              "Are you sure, you want to delete the user \"",
              user_name,
              "\"?"
            )
          ),
          footer = shiny::actionButton(
            inputId = ns("confirm_remove"),
            label = "Yes"
          )
        ))
      })

      shiny::observeEvent(input$confirm_remove, {
        shiny::removeModal()

        success <- db_remove_user(.values$db, user_name)

        if (success) {
          shiny::showNotification(
            ui = paste0(
              "The user \"",
              user_name,
              "\" was deleted succesfully."
            ),
            type = "warning",
            duration = 5
          )
        } else {
          shiny::showNotification(
            ui = paste0(
              "The user \"",
              user_name,
              "\" could not be deleted."
            ),
            type = "error",
            duration = 5
          )
        }

        .values$update$user(.values$update$user() + 1)
      })
    }
  )
}
