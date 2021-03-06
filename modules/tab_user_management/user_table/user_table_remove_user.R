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
        if (user_name == .values$user_rvs$name) {
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
          bs4Dash::toast(
            title = paste0(
              "The user \"",
              user_name,
              "\" was deleted succesfully."
            ),
            options = .values$settings$toast(
              class = "bg-warning"
            )
          )
        } else {
          bs4Dash::toast(
            title = paste0(
              "The user \"",
              user_name,
              "\" could not be deleted."
            ),
            options = .values$settings$toast(
              class = "bg-danger"
            )
          )
        }

        .values$update$db_user_rv(.values$update$db_user_rv() + 1)
      })
    }
  )
}
