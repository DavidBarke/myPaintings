#' Get All Transactions By User ID
#' 
#' @template db
#' @param user_id User ID.
#' 
#' @family transaction
#' 
#' @export
db_get_transactions_by_user_id_for_display <- function(db, user_id) {
  DBI::dbGetQuery(
    db,
    "SELECT image.title AS Title, buyer.name AS Buyer, seller.name AS Seller, 
    buy_sell.date AS Date, buy_sell.price AS Price
    FROM buy_sell
    INNER JOIN image ON buy_sell.image_id = image.rowid
    INNER JOIN user AS buyer ON buy_sell.buyer_id = buyer.rowid
    INNER JOIN user AS seller ON buy_sell.seller_id = seller.rowid
    WHERE buyer_id = ? OR seller_id = ? 
    ORDER BY buy_sell.date DESC",
    params = list(user_id, user_id)
  )
}
