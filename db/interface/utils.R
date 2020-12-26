#' Determine the length of a table
#'
#' Determine the length of a table, i.e. the number of records.
#'
#' @template db
#' @template name
#'
#' @export
db_length <- function(db, name) {
  DBI::dbGetQuery(db, paste("SELECT COUNT(*) AS n FROM", name))$n
}


#' Get All Columns of a Table Including Rowid
#'
#' Get the table of \code{db} with name \code{name}.
#'
#' @template db
#' @template name
#'
#' @export
db_get_table <- function(db, name) {
  fields <- c("rowid", DBI::dbListFields(db, name))
  
  query <- paste(
    "SELECT",
    paste(fields, collapse = ", "),
    "FROM",
    name
  )
  
  DBI::dbGetQuery(
    db,
    query
  )
}



#' Get Column of a Table
#' @template db
#' @template name
#' @param column Column name.
#' 
#' @export
db_get_column <- function(db, name, column) {
  DBI::dbGetQuery(
    db,
    paste(
      "SELECT",
      column,
      "FROM",
      name
    )
  )[[column]]
}