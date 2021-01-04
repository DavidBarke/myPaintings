dropdown_menu <- function(..., title) {
  htmltools::tagList(
    shiny::tags$a(
      href = "#",
      class = "nav-link dropdown-toggle",
      `data-toggle` = "dropdown",
      `aria-expanded` = "false",
      htmltools::tags$span(class = "d-none d-md-inline", title)
    ),
    htmltools::tags$ul(
      class = "dropdown-menu dropdown-menu-lg dropdown-menu-right dashboard-user",
      htmltools::tags$li(
        class = "dropdown-body",
        shiny::fluidRow(
          ...
        )
      )
    )
  )
}

dropdown_menu_output <- function(outputId) {
  shiny::uiOutput(
    outputId = outputId,
    container = htmltools::tags$li,
    class = "nav-item dropdown"
  )
}

dropdown_menu_item <- function(item, width) {
  htmltools::div(
    class = paste0("col-", width),
    item
  )
}