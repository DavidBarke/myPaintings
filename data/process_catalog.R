library(readxl)
library(writexl)
library(dplyr)
library(curl)

source("./data/process_catalog_helpers.R")

# Import ----
src_tbl <- read_excel("./data/catalog.xlsx")

# Transform ----
names(src_tbl) <- tolower(names(src_tbl))

src_tbl <- src_tbl %>% 
  filter(form == "painting") %>%
  mutate(painter_id = match(author, unique(author)))

url_tbl <- extract_url(src_tbl$url)

# Images ----
images <- bind_cols(
  src_tbl,
  url_tbl
) %>%
  select(-author, -`born-died`, -form) %>%
  filter(!is.na(url_title))

image_src <- src_path(images)
image_dest <- dest_path(images)

images$path <- str_match(image_dest, "./img/(.*)")[,2]

# Painters ----
painters <- src_tbl %>%
  select(painter_id, author, `born-died`) %>%
  distinct() %>%
  group_by(painter_id) %>%
  # Remove duplicates
  filter(row_number() == 1) %>%
  mutate(name = extract_name(author))

painters_life <- extract_life(painters$`born-died`)

painters <- bind_cols(
  painters,
  painters_life
) %>%
  select(-author, -`born-died`)

# Write ----
write_xlsx(images, "./data/images.xlsx")
write_xlsx(painters, "./data/painters.xlsx")

# WGA ----
if (FALSE) {
  add_letters_dirs()
  add_author_dirs(images)
  
  res <- download_wga(image_src, image_dest)
}

