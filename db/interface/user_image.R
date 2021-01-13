#' Get Image Owner
#' 
#' @template db
#' @param image_id Image ID.
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



#' Set Image Owner
#' 
#' @template db
#' @param image_id Image ID.
#' @param owner_id User ID of new owner.
#' 
#' @family user_image
#' 
#' @export
db_set_image_owner <- function(db, image_id, owner_id) {
  DBI::dbExecute(
    db,
    "UPDATE user_image
    SET user_id = ?
    WHERE image_id = ?",
    params = list(owner_id, image_id)
  )
}