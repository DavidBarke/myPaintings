library(stringr)
library(progress)

`%NA%` <- function(x, y) ifelse(!is.na(x), x, y)

format_painter <- function(first_name, last_name) {
  first_name <- first_name %NA% ""
  
  ifelse(nchar(first_name), paste(first_name, last_name), last_name)
}

extract_name <- function(author) {
  mat <- str_match(author, "([^,]*),?(.*)")
  
  first_name <- str_trim(mat[,3])
  last_name <- str_to_title(mat[,2])
  
  format_painter(first_name, last_name)
}

extract_life <- function(born_died) {
  mat <- str_match(born_died, "^\\(b\\.\\s(\\d+),\\s(\\w+),\\sd\\.\\s(\\d+),\\s(\\w+)\\)$")
  
  tibble <- tibble::tibble(
    year_born = as.integer(mat[,2]),
    year_died = as.integer(mat[,4]),
    location_born = mat[,3],
    location_died = mat[,5]
  )
}

extract_url <- function(url) {
  mat <- str_match(url, "^https://www.wga.hu/html/(\\w)/(\\w+)(.*)/([\\w-]+)\\.html$")
  
  mat_2 <- str_match(url, "^https://www.wga.hu.html/(.*)\\.html$")
  
  tibble::tibble(
    url_letter = mat[,2],
    url_author = mat[,3],
    url_title = mat[,5],
    url_path = mat_2[,2]
  )
}

add_letters_dirs <- function() {
  if (!file.exists("./img/wga")) dir.create("./img/wga")
  
  purrr::map(letters, function(letter) {
    dir_path <- file.path("./img/wga", letter)
    if (!file.exists(dir_path)) {
      dir.create(dir_path)
    }
  })
}

add_author_dirs <- function(tbl) {
  purrr::map2(tbl$url_author, tbl$url_letter, function(author, letter) {
    author_path <- file.path("./img/wga", letter, author)
    if (!file.exists(author_path)) {
      dir.create(author_path)
    }
  })
}

src_path <- function(tbl) {
  file.path(
    "https://www.wga.hu/art",
    paste0(tbl$url_path, ".jpg")
  )
}

dest_path <- function(tbl) {
  image <- paste0(tbl$url_title, ".jpg")
  
  file.path(
    "./img/wga",
    tbl$url_letter,
    tbl$url_author,
    image
  )
}

download_wga <- function(src, dest) {
  progress <- progress_bar$new(total = length(src))
  purrr::map2(src, dest, function(url, destfile) {
    progress$tick()
    download.file(url, destfile, mode = "wb", quiet = TRUE)
  })
}