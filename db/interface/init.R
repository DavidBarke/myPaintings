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
  source_directory(
    path = "./db/interface",
    encoding = "UTF-8",
    modifiedOnly = FALSE,
    chdir = TRUE,
    recursive = TRUE,
    envir = environment()
  )
  
  # Init DB
  db <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(db))
  
  # create tables
  create_user_table(db)
  create_image_table(db)
  create_user_image_table(db)
  create_offered_images_table(db)
  
  populate_user_table(db)
  populate_image_table(db)
  populate_user_image_table(db)
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
    title = character(),
    date = character(),
    technique = character(),
    location = character(),
    form = character(),
    type = character(),
    school = character(),
    timeframe = character(),
    first_name = character(),
    last_name = character(),
    url = character(),
    path = character()
  )
  
  DBI::dbCreateTable(db, "image", tbl)
}

create_user_image_table <- function(db) {
  tbl <- tibble::tibble(
    user_id = integer(),
    image_id = integer()
  )
  
  DBI::dbCreateTable(db, "user_image", tbl)
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
  n <- 100
  first_names <- randomNames::randomNames(n, which.names = "first")
  last_names <- randomNames::randomNames(n, which.names = "last")
  user_names <- c("Admin", paste(first_names, last_names))
  user_status <- c("admin", rep("user", times = n))
  user_password <- c("admin", tolower(first_names))
  user_password <- purrr::map_chr(user_password, ~ bcrypt::hashpw(.))
  
  purrr::pwalk(list(user_names, user_status, user_password), function(name, status, password) {
    db_add_user(db, name, status, password)
  })
}

populate_user_image_table <- function(db) {
  n <- db_length(db, "image")
  image_ids <- seq_len(n)
  user_ids <- sample(1:100, n, replace = TRUE)
  
  tbl <- tibble::tibble(
    image_id = image_ids,
    user_id = user_ids
  )
  
  DBI::dbAppendTable(db, "user_image", tbl)
}

#' @export
populate_image_table <- function(db) {
  tbl <- readxl::read_xlsx(
    "./data/images.xlsx"
  )
  
  fields <- DBI::dbListFields(db, "image")
  
  tbl <- tbl[fields]
  
  DBI::dbAppendTable(db, "image", tbl)
}
