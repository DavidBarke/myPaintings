#' Get All Painters
#' 
#' @template db
#' 
#' @family painter
#' 
#' @export
db_get_painters <- function(db, image_ids) {
  tbl <- DBI::dbGetQuery(
    db,
    "
    SELECT painter.painter_id, painter.name, image.rowid AS image_id
    FROM painter
    LEFT JOIN image ON painter.painter_id = image.painter_id
    "
  ) %>% dplyr::filter(
    image_id %in% image_ids
  )
  
  x <- tbl$painter_id
  names(x) <- tbl$name
  
  x
}