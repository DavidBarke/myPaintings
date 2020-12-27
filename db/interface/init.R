#' Initialise database
#'
#' Initialise SQLite database if not present.
#'
#' @param path Location at which database is created.
#'
#' @section Tables:
#' The database contains the following tables:
#'
#' * **User Table**
#'   * | **Column** | **Type** |
#'     | --- | --- |
#'     | name | character |
#'     | status | character |
#'     | password | character |
#'
#' @md
#' @export
db_init <- function(path = "db/db.sqlite") {
  # Init DB
  db <- DBI::dbConnect(RSQLite::SQLite(), path)
  
  # create tables
  create_user_table(db)
  create_image_table(db)
  create_offered_images_table(db)
  
  populate_user_table(db)
  populate_image_table(db)
  
  DBI::dbDisconnect(db)
}



#' @export
create_user_table <- function(db) {
  tbl <- tibble::tibble(
    name = character(),
    status = character(),
    password = character(),
    added_from = character(),
    time_added = character(),
    time_logged = character(),
    times_logged = integer(),
    capital = numeric()
  )
  
  DBI::dbCreateTable(db, "user", tbl)
}

#' @export
create_image_table <- function(db) {
  tbl <- tibble::tibble(
    path = character(),
    title = character(),
    original_title = character(),
    user_id = character(),
    year_start = numeric(),
    year_finish = numeric(),
    painter = character(),
    city = character(),
    museum = character()
  )
  
  DBI::dbCreateTable(db, "image", tbl)
}

create_offered_images_table <- function(db) {
  tbl <- tibble::tibble(
    image_id = character(),
    price = numeric()
  )
  
  DBI::dbCreateTable(db, "offered_images", tbl)
}



#' @export
populate_user_table <- function(db) {
  user_name <- c("Admin", "Alice", "Bob")
  user_status <- c("admin", "user", "user")
  user_password <- c("admin", "alice", "bob")
  user_password <- purrr::map_chr(user_password, ~ bcrypt::hashpw(.))
  
  purrr::pwalk(list(user_name, user_status, user_password), function(name, status, password) {
    db_add_user(db, name, status, password)
  })
}

#' @export
populate_image_table <- function(db) {
  tbl <- readxl::read_xlsx(
    "db/populate/image_table.xlsx"
  )
  
  tbl$path <- file.path("./img", tbl$path)
  
  DBI::dbAppendTable(db, "image", tbl)
}
