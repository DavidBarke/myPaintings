source("renv/activate.R")

if (file.exists("db/db.sqlite")) {
  db <- DBI::dbConnect(RSQLite::SQLite(), "db/db.sqlite")
}
