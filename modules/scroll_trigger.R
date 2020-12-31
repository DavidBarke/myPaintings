scroll_trigger <- function(inputId, threshold = 100) {
  func <- glue::glue(
    "$(window).scroll(
      function(e) {
        var win = $(window);
        var doc = $(document);
        var diff = doc.height() - win.scrollTop() - win.height();
        if (diff < win.height()) {
          console.log(diff);
          Shiny.setInputValue(\"[<inputId>]\", diff);
        }
      }
    )",
    inputId = inputId,
    threshold = threshold,
    .open = "[<",
    .close = ">]"
  )
  
  htmltools::tags$script(
    htmltools::HTML(func)
  )
}
