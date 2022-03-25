library(tidyverse)
library(reluxr)

fl <- "inst/testdata/20220323_DeconvoPlate_384.csv"
fl2 <- "inst/testdata/test-data.xlsm"



df <- data.table::fread(fl,
                  skip = 383, nrows = 417 - 383, header = TRUE) %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  select(-v1) %>%
  pivot_longer(cols = matches("^\\w\\d+$"),
               names_to = "well",
               values_to = "lum")


col1 <- data.table::fread(fl)[, c(1,2)]
# data.table::fread(fl) %>%
df <-
  col1 %>%
  as_tibble() %>%
  rownames_to_column() %>%
  mutate(
    time_entry = str_detect(lead(V2), "\\d{1,2}:\\d{1,2}:\\d{1,2}"),
    time = str_detect(V2, "Time") & time_entry | str_detect(V1, "Results"),
    type = lag(V1, 2)
  ) %>%
  filter(time) %>%
  arrange(as.numeric(rowname)) %>%
  mutate(
    rowname = as.numeric(rowname),
    start = rowname,
    end = lead(start) - 1
  ) %>%
  drop_na() %>%
  select(type, start, end) %>%
  filter(str_detect(type, "LUM")) %>%
  group_by(type) %>%
  mutate(
    data = map2(start, end, \(x, y) {
      readxl::read_excel(fl2, skip = x -1, n_max = y - x) %>%
        janitor::clean_names()
    })
  )



df %>%
  select(type, data) %>%
  # .[1, ] %>%
  # unnest(data) %>%


  group_by(type) %>%
  ungroup() %>%
  mutate(data = map(data, as_tibble)) %>%
  mutate(
    data = map(data, function(x) {
      x %>%
        select(seq(str_which(colnames(.), "time"), ncol(.))) %>%
        pivot_longer(
          cols = matches("^\\w\\d{1,3}$"),
          values_to = "lum",
          names_to = "well"
        )
    })
  ) %>%
  unnest(data) %>%
  mutate(

  ) %>%
  ggplot(aes(time, lum, group = well)) +
  geom_line() +
  scale_y_log10()





hours_since_start <- function(time) {
  as.numeric(difftime(
    as.POSIXct(time, format = "%H:%M:%S"),
    as.POSIXct("0:00:00", format = "%H:%M:%S"),
    units = "hours"
  ))
}

