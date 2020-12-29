library(stringr)

extract_name <- function(author) {
  mat <- str_match(author, "([^,]*),?(.*)")
  
  tibble::tibble(
    first_name = mat[,3],
    last_name = str_to_title(mat[,2])
  )
}

extract_life <- function(born_died) {
  mat <- str_match(born_died, "^\\(b\\.\\s(\\d+),\\s(\\w+),\\sd\\.\\s(\\d+),\\s(\\w+)\\)$")
  
  tibble <- tibble::tibble(
    year_born = mat[,2],
    year_died = mat[,4],
    location_born = mat[,3],
    location_died = mat[,5]
  )
}

extract_url <- function(url) {
  mat <- str_match(url, "https://www.wga.hu/html/(\\w)/(\\w+)/.*(\\w+)\\.html$")
  
  tibble::tibble(
    url_letter = mat[,2],
    url_author = mat[,3],
    url_title = mat[,4]
  )
}

add_letters_dirs <- function() {
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
  image <- paste0(tbl$url_title, ".jpg")
  
  file.path(
    "https://www.wga.hu/art",
    tbl$url_letter,
    tbl$url_author,
    image
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

download_wga <- function(tbl) {
  purrr::map(seq_len(nrow(tbl)), function(i) {
    
  })
}