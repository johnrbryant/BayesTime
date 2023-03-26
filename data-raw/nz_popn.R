
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(poputils)

age_labels <- age_labels(type = "single", max = 95)
col_names <- c("time",
               paste("Male", age_labels, sep = "."),
               paste("Female", age_labels, sep = "."))
col_types <- paste(rep(c("c", "d", "-", "d", "-", "d", "-", "d", "-"),
                       times = c(1, 95, 38, 1, 2, 95, 38, 1, 2)),
                       collapse = "")

nz_popn <- read_csv("data-raw/DPE403905_20230322_103055_56.csv",
                 skip = 4,
                 n_max = 32,
                 na = "..",
                 col_types = col_types,
                 col_names = col_names) %>%
    pivot_longer(cols = -time,
                 names_to = c("sex", "age"),
                 names_sep = "\\.") %>%
    mutate(age = collapse_age(age, type_to = "five")) %>%
    count(age, sex, time, wt = value, name = "count")

save(nz_popn, file = "data/nz_popn.rda", compress = "bzip2")

