#' Add Image To Image Table
#' 
#' @template db
#' @param user_id User id.
#' @param title Title of image.
#' 
#' @family image
#' 
#' @export
db_add_image <- function(db, user_id, title) {
  entry <- tibble::tibble(
    title = title,
    user_id = user_id
  )
  
  DBI::dbAppendTable(db, "image", entry)
}



#' Get All Images
#' 
#' @template db
#' 
#' @family image
#' 
#' @export
db_get_images <- function(db) {
  tbl <- DBI::dbGetQuery(
    db,
    "SELECT rowid AS image_id, * FROM image"
  )
  
  x <- tbl$row_id
  names(x) <- tbl$title
  
  x
}



#' Get All Images from User
#' 
#' @template db
#' @param user_id User id.
#' 
#' @family image
#' 
#' @export
db_get_images_by_user_id <- function(db, user_id) {
  tbl <- DBI::dbGetQuery(
    db,
    "SELECT rowid AS image_id, * FROM image WHERE user_id = ?",
    params = list(user_id)
  )
  
  x <- tbl$rowid
  names(x) <- tbl$title
  
  x
}



#' Get Image Entry by Image ID
#' 
#' @template db
#' @param image_id Image id.
#' 
#' @family image
#' 
#' @export
db_get_image_entry_by_image_id <- function(db, image_id) {
  DBI::dbGetQuery(
    db,
    "SELECT rowid AS image_id, * FROM image WHERE rowid = ?",
    params = list(image_id)
  )
}



#' Set Image Title
#' 
#' @template db
#' @param image_id Image id.
#' @param title New image title.
#' 
#' @family image
#' 
#' @export
db_set_image_title <- function(db, image_id, title) {
  DBI::dbExecute(
    db,
    "UPDATE image SET title = ? WHERE rowid = ?",
    params = list(title, image_id)
  )
}