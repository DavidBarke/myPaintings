#' Offer Image for Sale
#' 
#' @template db
#' @param image_id Image id.
#' @param price Image price.
#' 
#' @family offered_images
#' 
#' @export
db_offer_image <- function(db, image_id, price) {
  entry <- tibble::tibble(
    image_id = image_id,
    price = price
  )
  
  DBI::dbAppendTable(db, "offered_images", entry)
}



#' Withdraw Image from Sale
#' 
#' @template db
#' @param image_id Image id.
#' 
#' @family offered_images
#' 
#' @export
db_withdraw_offer_image <- function(db, image_id) {
  DBI::dbExecute(
    db,
    "DELETE FROM offered_images WHERE image_id = ?",
    params = list(as.integer(image_id))
  )
}



#' Determine if Image is Offered
#' 
#' @template db
#' @param image_id Image id.
#' 
#' @family offered_images
#' 
#' @export
db_is_image_offered <- function(db, image_id) {
  tbl <- DBI::dbGetQuery(
    db,
    "SELECT image_id FROM offered_images WHERE image_id = ?",
    params = list(as.integer(image_id))
  )
  
  image_id %in% tbl$image_id
}



#' Get Offered Price for Image
#' 
#' @template db
#' @param image_id Image id.
#' 
#' @family offered_images
#' 
#' @export
db_get_offered_price <- function(db, image_id) {
  DBI::dbGetQuery(
    db,
    "SELECT price FROM offered_images WHERE image_id = ?",
    params = list(as.integer(image_id))
  )$price
}



#' Set Offered Price for Image
#' 
#' @template db
#' @param image_id Image id.
#' 
#' @family offered_images
#' 
#' @export
db_set_offered_price <- function(db, image_id, price) {
  DBI::dbExecute(
    db,
    "UPDATE offered_images SET price = ? WHERE image_id = ?",
    params = list(price, as.integer(image_id))
  )
}