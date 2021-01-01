#' Get Image Owner
#' 
#' @template db
#' @param image_id Image id.
#' 
#' @family user_image
#' 
#' @export
db_get_image_owner <- function(db, image_id) {
  DBI::dbGetQuery(
    db,
    "SELECT user.name
    FROM user_image
    INNER JOIN user
    ON user_image.user_id = user.rowid
    WHERE image_id = ?",
    params = list(image_id)
  )$name
}
