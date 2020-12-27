library(readxl)
library(dplyr)

source("./data/process_catalog_helpers.R")

# Import ----
tbl <- read_excel("./data/catalog.xlsx")

# Transform ----
artists <- tbl %>%
  select(author = AUTHOR, born_died = `BORN-DIED`) %>%
  distinct(author, born_died)
