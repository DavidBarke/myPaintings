title_box <- function(title, subtitle, width, status) {
  shiny::column(
    width = width,
    htmltools::div(
      class = paste("jumbotron", paste("bg", status, sep = "-")),
      htmltools::h1(
        class = "display-4",
        title
      ),
      htmltools::p(
        class = "lead",
        subtitle
      ),
      htmltools::p(
        solidheader="TRUE",
        collapsible="FALSE"
      )
    )
  )
}