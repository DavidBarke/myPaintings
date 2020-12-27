`%_%` <- function(x, y) paste0(x, "_", y)

`%||%` <- function(x, y) if (!is.null(x)) x else y

`%NA%` <- function(x, y) if (!is.na(x)) x else y

as_english <- function(x) {
  dict <- c(
    "1"  = "one",
    "2"  = "two",
    "3"  = "three",
    "4"  = "four",
    "5"  = "five",
    "6"  = "six",
    "7"  = "seven",
    "8"  = "eight",
    "9"  = "nine",
    "10" = "ten",
    "11" = "eleven",
    "12" = "twelve"
  )

  if (x <= 12) {
    dict[as.character(x)]
  } else {
    x
  }
}

format_currency <- function(x, currency = "$") {
  DT::formatCurrency(
    table = DT::datatable(tibble::tibble(x = x)), 
    columns = "x",
    currency = currency
  )$x
}

format_museum <- function(museum, city) {
  if (is.na(museum)) return(city %NA% "Missing")
  
  if (is.na(city)) return(museum)
  
  paste(museum, city, sep = ", ")
}


