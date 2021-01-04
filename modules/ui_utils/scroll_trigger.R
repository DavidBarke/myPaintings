scroll_trigger <- function(inputId, threshold = 100) {
  func <- glue::glue(
    "$(window).scroll(
      function(e) {
        var win = $(window);
        var doc = $(document);
        var diff = doc.height() - win.scrollTop() - win.height();
        var last_height = $(\"#[<inputId>]\").attr(\"last-height\");
        var movement = doc.height() - last_height;
        if (diff < 1.5 * win.height() && movement > 0) {
          $(\"#[<inputId>]\").attr(\"last-height\", doc.height());
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
    id = inputId,
    `last-height` = 0,
    htmltools::HTML(func)
  )
}
