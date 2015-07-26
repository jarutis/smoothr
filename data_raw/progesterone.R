##  This is progesterone data from Brumback and Rice (1998)
##      Group: nonconceptive 0; conceptive 1
##      Subject: woman under study
##      Cycle  : the cycles of a woman
##      Day    : negative before ovulation; positive after ovulation
##      Missing:  0---not missing 1---missing
##      Reorganized by Jin-Ting Zhang National University of Singapore
##      Ported to R by Jonas Jarutis
##      Jul 25, 2015

library(dplyr)

url <- "http://www.stat.nus.edu.sg/~zhangjt/books/Chapman/datasets/rawdata/prog.txt"
system(
  paste0(
    "wget -qO- '", url ,"' |",
    "sed -Ee '1,9d' -e 's/^ +//' -e 's/ +/,/g' > data_raw/prog.csv"
  )
)

progesterone_raw <- read.csv("data_raw/prog.csv", header=FALSE)
names(progesterone_raw) <- c("group", "subject", "cycle", "day", "progesterone", "missing")

progesterone <- progesterone_raw %>%
  mutate(group=ifelse(group == 0, "nonconceptive", "conceptive") %>% factor) %>%
  mutate(progesterone=ifelse(missing == 1, NA, progesterone)) %>%
  mutate(subject=subject %>% factor) %>%
  mutate(cycle=cycle %>% factor) %>%
  select(-missing) %>%
  as.data.frame

devtools::use_data(progesterone, overwrite=TRUE)
