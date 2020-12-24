dt_options <- function() {
  options(
    DT.options = list(
      pageLength = 5,
      language = list(
        url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/German.json'
      ),
      dom = "fpt",
      scrollX = TRUE
    )
  )
}
