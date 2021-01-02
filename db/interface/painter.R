#' Get All Painters
#' 
#' @template db
#' 
#' @family painter
#' 
#' @export
db_get_painter <- function(db) {
  tbl <- DBI::dbGetQuery(
    db,
    "SELECT painter_id, first_name, last_name FROM painter"
  )
  
  x <- tbl$painter_id
  names(x) <- format_painter(tbl$first_name, tbl$last_name)
  
  x
}