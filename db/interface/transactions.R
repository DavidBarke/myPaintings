#' Get Transaction IDs By User ID
#' 
#' @template db
#' @param user_id User ID.
#' 
#' @family transaction
#' 
#' @export
db_get_transaction_ids_by_filter <- function(db, user_id, date_start, date_end) {
  date_start <- as.character(date_start)
  date_end <- as.character(date_end)
  DBI::dbGetQuery(
    db,
    "SELECT rowid
    FROM buy_sell
    WHERE (buyer_id = ? OR seller_id = ?) AND date > ? AND date < ?",
    params = list(user_id, user_id, date_start, date_end)
  )$rowid
}

db_get_transaction_display <- function(db, transaction_ids) {
  tbl <- DBI::dbGetQuery(
    db,
    "SELECT image.title AS Title, buyer.name AS Buyer, seller.name AS Seller, 
    buy_sell.date AS Date, buy_sell.price AS Price
    FROM buy_sell
    INNER JOIN image ON buy_sell.image_id = image.rowid
    INNER JOIN user AS buyer ON buy_sell.buyer_id = buyer.rowid
    INNER JOIN user AS seller ON buy_sell.seller_id = seller.rowid
    WHERE buy_sell.rowid = ?",
    params = list(transaction_ids)
  )
  
  # sort "manually" as indexed query can't be used with ORDER BY
  tibble::as_tibble(tbl) %>%
    dplyr::arrange(desc(Date))
}
