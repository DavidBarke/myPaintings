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



#' Get All Images from User
#' 
#' @template db
#' @param user_id User id.
#' 
#' @family image
#' 
#' @export
db_get_image_ids_by_user_id <- function(db, user_id) {
  DBI::dbGetQuery(
    db, 
    "SELECT image.rowid AS image_id, user_image.user_id
    FROM image 
    INNER JOIN user_image 
    ON image.rowid = user_image.image_id 
    WHERE user_image.user_id = ?", 
    params = list(user_id)
  )$image_id
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
    "SELECT image.rowid AS image_id, image.title, image.date, image.technique,
      image.location, image.type, image.school, image.timeframe, image.url,
      image.path, painter.name, painter.year_born,
      painter.year_died, painter.location_born, painter.location_died
    FROM image 
    INNER JOIN painter
    ON image.painter_id = painter.painter_id
    WHERE image.rowid = ?",
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



#' Get Image IDs
#' 
#' @template db
#' 
#' @family image
#' 
#' @export
db_get_image_ids <- function(db) {
  tbl <- DBI::dbGetQuery(
    db,
    "SELECT rowid, title FROM image"
  )
  
  x <- tbl$rowid
  names(x) <- tbl$title
  
  x
}



#' Get All Image Schools
#' 
#' @template db
#' 
#' @family image
#' 
#' @export
db_get_image_schools <- function(db, image_ids) {
  DBI::dbGetQuery(
    db,
    "SELECT school, rowid AS image_id FROM image"
  ) %>% dplyr::filter(
    image_id %in% image_ids
  ) %>% `[[`(
    1
  ) %>%
    unique()
}



#' Get All Image Types
#' 
#' @template db
#' 
#' @family image
#' 
#' @export
db_get_image_types <- function(db, image_ids) {
  DBI::dbGetQuery(
    db,
    "SELECT type, rowid AS image_id FROM image"
  ) %>% dplyr::filter(
    image_id %in% image_ids
  ) %>% `[[`(
    1
  ) %>%
    unique()
}