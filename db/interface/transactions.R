#' Get All Transactions By User ID
#' 
#' @template db
#' @param user_id User ID.
#' 
#' @family transaction
#' 
#' @export
db_get_transactions_by_user_id <- function(db, user_id) {
  DBI::dbGetQuery(
    db,
    "SELECT * FROM buy_sell WHERE buyer_id = ? OR seller_id = ? ORDER BY date DESC",
    params = list(user_id, user_id)
  )
}
