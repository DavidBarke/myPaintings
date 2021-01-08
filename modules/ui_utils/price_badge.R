price_badge <- function(price) {
  bs4Dash::bs4Badge(
    class = "price-badge",
    scales::dollar_format()(price),
    color = "danger"
  )
}