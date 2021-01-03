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
  create_painter_table(db)
  create_user_image_table(db)
  create_offered_images_table(db)
  create_buy_sell_table(db)
  
  populate_user_table(db)
  populate_image_table(db)
  populate_painter_table(db)
  populate_user_image_table(db)
  populate_buy_sell_table(db)
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
    painter_id = integer(),
    date = character(),
    technique = character(),
    location = character(),
    type = character(),
    school = character(),
    timeframe = character(),
    url = character(),
    path = character()
  )
  
  DBI::dbCreateTable(db, "image", tbl)
}

#' @export
create_painter_table <- function(db) {
  tbl <- tibble::tibble(
    # Don't use rowid
    painter_id = integer(),
    name = character(),
    year_born = integer(),
    year_died = integer(),
    location_born = character(),
    location_died = character()
  )
  
  DBI::dbCreateTable(db, "painter", tbl)
}

#' @export
create_user_image_table <- function(db) {
  tbl <- tibble::tibble(
    user_id = integer(),
    image_id = integer()
  )
  
  DBI::dbCreateTable(db, "user_image", tbl)
}

#' @export
create_offered_images_table <- function(db) {
  tbl <- tibble::tibble(
    image_id = character(),
    price = numeric()
  )
  
  DBI::dbCreateTable(db, "offered_images", tbl)
}

#' @export
create_buy_sell_table <- function(db) {
  tbl <- tibble::tibble(
    image_id = integer(),
    seller_id = integer(),
    buyer_id = integer(),
    price = integer(),
    date = character()
  )
  
  DBI::dbCreateTable(db, "buy_sell", tbl)
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

#' @export
populate_image_table <- function(db) {
  tbl <- readxl::read_xlsx(
    "./data/images.xlsx"
  )
  
  fields <- DBI::dbListFields(db, "image")
  
  tbl <- tbl[fields]
  
  DBI::dbAppendTable(db, "image", tbl)
}

#' @export
populate_painter_table <- function(db) {
  tbl <- readxl::read_xlsx(
    "./data/painters.xlsx"
  )
  
  fields <- DBI::dbListFields(db, "painter")
  
  tbl <- tbl[fields]
  
  DBI::dbAppendTable(db, "painter", tbl)
}

#' @export
populate_user_image_table <- function(db) {
  n <- db_length(db, "image")
  image_ids <- seq_len(n)
  user_ids <- sample(db_get_user_ids(db), n, replace = TRUE)
  
  tbl <- tibble::tibble(
    image_id = image_ids,
    user_id = user_ids
  )
  
  DBI::dbAppendTable(db, "user_image", tbl)
}

#' @export
populate_buy_sell_table <- function(db) {
  # Every user has bought 100 images
  user_ids <- db_get_user_ids(db)
  
  tbl <- purrr::map_dfr(user_ids, function(user_id) {
    image_ids <- db_get_image_ids_by_user_id(db, user_id)
    bought_image_ids <- sample(image_ids, 100, replace = FALSE)
    seller_ids <- sample(user_ids[user_ids != user_id], 100, replace = TRUE)
    tibble::tibble(
      image_id = bought_image_ids,
      seller_id = seller_ids,
      buyer_id = user_id
    )
  })
  
  dates <- seq(lubridate::ymd_hms("2019-01-01 00:00:00"), lubridate::ymd_hms("2020-12-31 23:59:59"), by = "sec")
  # expected value is 2e5
  price <- round(1e5 * rchisq(nrow(tbl), df = 2), digits = 2)
  
  tbl$date <- as.character(sample(dates, nrow(tbl), replace = TRUE))
  tbl$price <- price
  
  DBI::dbAppendTable(db, "buy_sell", tbl)
}
