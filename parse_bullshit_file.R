library(reluxr)
library(tidyverse)

fl <- "inst/newfiles/20220323_DeconvoPlate_384.csv"
counter <- 0
out_list <- list()

last_line_details <- "End Kinetic"

find_end_of_metadata <- function(file) {
  max(which(readLines(file) %>% str_detect(last_line_details)))
}

# find_end_of_metadata(fl)
end_met <- find_end_of_metadata(fl)
raw_readings <- read_csv(fl, skip = end_met)
raw_rows <- readLines(fl)
raw_rows <- raw_rows[seq(end_met, length(raw_rows))]
raw_rows <- raw_rows %>%
  str_remove_all("T\\xb0 ")
col_1 <- raw_readings %>%
  pull(1)
unique_headings <- raw_readings %>%
  drop_na(1) %>%
  pull(1) %>%
  unique()

which(!is.na(col_1))

df <- tibble(
  raw = raw_rows
) %>%
  slice(-c(1,2)) %>%
  ungroup() %>%
  mutate(
    row = seq(nrow(.)),
    group = cut(row, breaks = which(!is.na(col_1)) - 1,
                include.lowest = FALSE, labels = FALSE)
  ) %>%
  group_by(group) %>%
  nest()

# df %>%
#   mutate(
#     data = map(data, function(x) {
#       str_replace_all(x, fixed("T\xb0 "), "")
#     })
#   ) %>%
#   unnest(data)

df <- df %>%
  mutate(
    group = if_else(is.na(group), 17L, group)
  )

df <- df %>%
  mutate(
    chunk = map_chr(data, function(x) {
      str_extract(x$raw[1], "^[^,]+")
    })
  ) %>%

  mutate(
    values = map(data, function(x) {
      y <- x %>%
        slice(-c(1, 2)) %>%
        .$raw


      fl1 <- tempfile()
      writeLines(y, print(fl1))
      read_csv(fl1) %>%
        select(-1)
    })
  )




df %>%
  select(chunk, values) %>%
  mutate(
    values = map(values, function(x) {
      x %>%
        pivot_longer(cols = matches("^\\w\\d{1,2}$")) %>%
        janitor::clean_names()
    })
  ) %>%
  ungroup() %>%
  filter(chunk != "Results") %>%
  unnest(values)
