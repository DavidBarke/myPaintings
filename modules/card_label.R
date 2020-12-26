card_label <- function(text, status, tooltip = NULL) {
  shiny::tags$span(
    class = paste0("badge bg-", status), 
    title = if (!is.null(tooltip)) tooltip, 
    `data-toggle` = if (!is.null(tooltip)) "tooltip", 
    text
  )
}