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
    user_id = character()
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
  tbl <- tibble::tribble(
    ~path, ~title, ~user_id,
    "mona_lisa.png", "Mona Lisa", 1,
    "last_supper.png", "Last Supper", 1,
    "the_scream.jpg", "The Scream", 1,
    "three_flowers.jpg", "Three Flowers", 1,
    "five_flowers.jpg", "Five Flowers", 1,
    "twelve_flowers.jpg", "Twelve Flowers", 1,
    "water_lily_pond.jpg", "Water-Lily Pond", 1,
    "soleil_levant.jpg", "Rising Sun", 1,
    "creation_of_adam.jpg", "Creation of Adam", 1,
    "starry_night.jpg", "Starry Night", 1, 
    "girl_with_a_pearl_earring.jpg", "Girl with a Pearl Earring", 1,
    "the_birth_of_venus.jpg", "The Birth of Venus", 1, 
    "the_kiss.jpg", "The Kiss", 1
  )
  tbl$path <- file.path("./img", tbl$path)
  
  DBI::dbAppendTable(db, "image", tbl)
}
