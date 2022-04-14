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


df %>%
  mutate(
    chunk = map_chr(data, function(x) {
      str_extract(x$raw[1], "^[^,]+")
    })
  ) %>%


df$data[[1]]$raw
lapply(df$data, \(x) x)
df %>%
  filter(is.na(group)) %>%
  unnest()

df %>%
  mutate(
    tib = map(data, function(x) {
      y <- x %>%
        slice(-c(1, 2)) %>%
        .$raw


      fl1 <- tempfile()
      writeLines(y, print(fl1))
      read_csv(fl1)
    })
  ) %>%
  unnest(tib)

