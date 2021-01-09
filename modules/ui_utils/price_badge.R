price_badge <- function(price, id) {
  bs4Dash::bs4Badge(
    class = "price-badge",
    shiny::actionLink(
      inputId = id,
      label = scales::dollar_format()(price)
    ),
    color = "danger"
  )
}