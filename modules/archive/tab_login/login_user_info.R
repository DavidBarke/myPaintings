login_user_info_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tagList(
    shiny::uiOutput(
      outputId = ns("user_name")
    ),
    shiny::uiOutput(
      outputId = ns("user_logged_since")
    ),
    shiny::uiOutput(
      outputId = ns("user_last_logged")
    ),
    shiny::uiOutput(
      outputId = ns("user_times_logged")
    )
  )
}

login_user_info_server <- function(id, .values) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      timer_r <- shiny::reactiveTimer(intervalMs = 5000)

      is_logged_r <- shiny::reactive({
        .values$user_rv()$status != "not_logged"
      })

      output$user_name <- shiny::renderUI({
        if (is_logged_r()) {
          bs4Dash::infoBox(
            title = .values$settings$status_dict[.values$user_rv()$status],
            value = .values$user_rv()$name,
            icon = shiny::icon("users"),
            color = "primary",
            width = NULL
          )
        }
      })

      diff_time_since_r <- shiny::reactive({
        .values$update$db_user_rv()
        timer_r()

        current_logged_time <- db_get_user_last_logged(
          db = .values$db,
          name = .values$user_rv()$name
        )

        current_logged_time <- lubridate::ymd_hms(current_logged_time)
        current_time <- lubridate::ymd_hms(Sys.time())

        diff_time <- current_time - current_logged_time
      })

      logged_since_r <- shiny::reactive({
        unit <- .values$settings$time_unit_dict[attr(diff_time_since_r(), "units")]
        value <- round(as.numeric(diff_time_since_r()))
        paste(value, unit)
      })

      output$user_logged_since <- shiny::renderUI({
        if (is_logged_r()) {
          bs4Dash::infoBox(
            title = "Logged in for",
            value = htmltools::div(
              class = "relative",
              logged_since_r()
            ),
            icon = shiny::icon("user-clock"),
            color = "primary",
            width = NULL
          )
        }
      })

      diff_time_last_r <- shiny::reactive({
        timer_r()

        last_logged_time <- lubridate::ymd_hms(.values$user_rv()$last_logged)
        current_time <- lubridate::ymd_hms(Sys.time())

        diff_time <- current_time - last_logged_time
      })

      last_logged_r <- shiny::reactive({
        unit <- .values$settings$time_unit_dict[attr(diff_time_last_r(), "units")]
        value <- round(as.numeric(diff_time_last_r()))
        paste(value, unit)
      })

      output$user_last_logged <- shiny::renderUI({
        if (is_logged_r()) {
          bs4Dash::infoBox(
            title = "Last logged in before",
            value = htmltools::div(
              class = "relative",
              last_logged_r()
            ),
            icon = shiny::icon("history"),
            color = "primary",
            width = NULL
          )
        }
      })

      times_logged_r <- shiny::reactive({
        db_get_user_times_logged(.values$db, .values$user_rv()$name)
      })

      output$user_times_logged <- shiny::renderUI({
        if (is_logged_r()) {
          bs4Dash::infoBox(
            title = "Number of visits",
            value = htmltools::div(
              class = "relative",
              times_logged_r()
            ),
            icon = shiny::icon("door-open"),
            color = "primary",
            width = NULL
          )
        }
      })
    }
  )
}
