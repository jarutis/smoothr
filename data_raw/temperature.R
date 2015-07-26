## Canadian Temperature Data
library(readr)
library(dplyr)

temperature_raw <- read_table(
  file="http://www.stat.nus.edu.sg/~zhangjt/books/Chapman/datasets/rawdata/temp.txt",
  skip=8,
  col_names=c("group", "daytime", "temperature"),
  col_type=list(col_double(), col_double(), col_double())
)

temperature <- temperature_raw %>%
  mutate(group=ifelse(group == 1, "East", ifelse(group == 2, "West", "North"))) %>%
  mutate(curve=cumsum(ifelse(daytime == 0.5, 1, 0)) %>% factor) %>%
  mutate(group=group %>% factor) %>%
  select(group, curve, daytime, temperature) %>%
  as.data.frame

devtools::use_data(temperature, overwrite=TRUE)
