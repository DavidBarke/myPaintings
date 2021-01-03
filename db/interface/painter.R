#' Get All Painters
#' 
#' @template db
#' 
#' @family painter
#' 
#' @export
db_get_painters <- function(db) {
  tbl <- DBI::dbGetQuery(
    db,
    "SELECT painter_id, name FROM painter"
  )
  
  x <- tbl$painter_id
  names(x) <- tbl$name
  
  x
}