scroll_trigger <- function(inputId, containerId, threshold = 100) {
  func <- glue::glue(
    "$(window).scroll(
      function(e) {
        var win = $(window);
        var doc = $(document);
        var diff = doc.height() - win.scrollTop() - win.height();
        var busy = $(\"html\").hasClass(\"shiny-busy\");
        var hasSpinner = $(\"#[<containerId>]\").find(\".load-container\").not(\".shiny-spinner-hidden\").length !== 0;
        if (diff < 1.5 * win.height() && !busy && !hasSpinner) {
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
    htmltools::HTML(func)
  )
}
