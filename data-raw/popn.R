
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

popn <- read_csv("DPE403905_20230322_103055_56.csv",
                    skip = 5, ## skip values for 1991, which have NAs
                    n_max = 31,
                    na = "..",
                    col_types = col_types,
                    col_names = col_names) %>%
    pivot_longer(cols = -time,
                 names_to = c("sex", "age"),
                 names_sep = "\\.") %>%
    mutate(age = collapse_age(age, type_to = "five")) %>%
    count(age, sex, time, wt = value, name = "count")


check_totals <- read_csv("DPE403905_20230322_103055_56.csv",
                         skip = 5,
                         n_max = 31,
                         na = "..",
                         col_types = paste(rep(c("-", "d", "-", "d"),
                                               times = c(136, 1, 135, 1)),
                                           collapse = ""),
                         col_names = c("Male", "Female")) %>%
    colSums()

ans_obtained <- count(popn, sex, wt = count)$n
ans_expected <- rev(as.numeric(check_totals))
stopifnot(isTRUE(all.equal(ans_obtained, ans_expected, tolerance = 0.001)))
                           

saveRDS(popn, file = "popn.rds")

