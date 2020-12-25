user_table_ui <- function(id) {
  ns <- shiny::NS(id)

  bs4Dash::box(
    width = NULL,
    status = "primary",
    title = "User table",
    solidHeader = TRUE,
    DT::dataTableOutput(
      outputId = ns("user_table")
    )
  )
}

user_table_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      taken_user_names_rvs <- shiny::reactiveValues(
        change_status = character(),
        remove = character(),
        reset_password = character()
      )

      output$user_table <- DT::renderDataTable({
        .values$update$user()

        tbl <- db_get_table(.values$db, "user")

        tbl$change_status <- purrr::map2_chr(
          tbl$name, tbl$status, function(user_name, status) {
            if (!user_name %in% taken_user_names_rvs$change_status) {
              taken_user_names_rvs$change_status <- c(
                taken_user_names_rvs$change_status, user_name
              )

              user_table_change_status_server(
                id = "user_table_change_status" %_% user_name,
                .values = .values,
                user_name = user_name,
                status = status
              )
            }

            user_table_change_status_ui(
              id = ns("user_table_change_status" %_% user_name)
            )
          }
        )

        tbl$remove <- purrr::pmap_chr(
          list(tbl$name, tbl$status, tbl$added_from),
          function(user_name, status, added_from) {
            if (!user_name %in% taken_user_names_rvs$remove) {
              taken_user_names_rvs$remove <- c(
                taken_user_names_rvs$remove, user_name
              )

              user_table_remove_user_server(
                id = "user_table_remove_user" %_% user_name,
                .values = .values,
                user_name = user_name,
                status = status,
                added_from = added_from
              )
            }

            user_table_remove_user_ui(
              id = ns("user_table_remove_user" %_% user_name)
            )
          }
        )

        tbl$reset_password <- purrr::map_chr(tbl$name, function(user_name) {
          if (!user_name %in% taken_user_names_rvs$reset_password) {
            taken_user_names_rvs$reset_password <- c(
              taken_user_names_rvs$reset_password, user_name
            )

            user_table_reset_password_server(
              id = "user_table_reset_password" %_% user_name,
              .values = .values,
              user_name = user_name
            )
          }

          user_table_reset_password_ui(
            id = ns("user_table_reset_password" %_% user_name)
          )
        })

        tbl <- tbl %>%
          dplyr::select(name, status, change_status, remove, reset_password) %>%
          dplyr::mutate(status = .values$settings$status_dict[status])

        tbl <- tbl[rev(seq_len(nrow(tbl))), , drop = FALSE]

        DT::datatable(
          data = tbl,
          options = list(
            pageLength = 6,
            columnDefs = list(
              list(
                className = 'dt-center',
                targets = 3:5
              )
            )
          ),
          escape = FALSE,
          colnames = c(
            "Username", "Status", "Change status", "Delete user",
            "Reset password"
          )
        )
      })
    }
  )
}
