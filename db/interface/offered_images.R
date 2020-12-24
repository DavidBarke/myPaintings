#' Offer Image for Sale
#' 
#' @template db
#' @param image_id Image id.
#' @param price Image price.
#' 
#' @family sell_image
#' 
#' @export
db_offer_image <- function(db, image_id, price) {
  entry <- tibble::tibble(
    image_id = image_id,
    price = price
  )
  
  DBI::dbAppendTable(db, "offered_images", entry)
}