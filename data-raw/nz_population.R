
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(poputils)

levels_time_keep <- 1992:2021
levels_age_keep <- age_labels(type = "five", min = 15, max = 65, open = TRUE)

age_labels <- age_labels(type = "single", max = 95)
col_names <- c("time",
               paste("Male", age_labels, sep = "."),
               paste("Female", age_labels, sep = "."))
col_types <- paste(rep(c("c", "d", "-", "d", "-", "d", "-", "d", "-"),
                       times = c(1, 95, 38, 1, 2, 95, 38, 1, 2)),
                       collapse = "")
levels_time <- 1992:2021
age_max <- 65

nz_population <- read_csv("DPE403905_20230322_103055_56.csv.gz",
                          skip = 5, ## skip values for 1991, which have NAs
                          n_max = 31,
                          na = "..",
                          col_types = col_types,
                          col_names = col_names) %>%
    pivot_longer(cols = -time,
                 names_to = c("sex", "age"),
                 names_sep = "\\.") %>%
    filter(time %in% levels_time_keep) %>%
    mutate(time = as.integer(time)) %>%
    mutate(age = clean_age(age),
           age = collapse_age(age, type_to = "five"),
           age = set_age_open(age, lower = 65)) %>%
    filter(age %in% levels_age_keep) %>%
    droplevels() %>%
    count(age, sex, time, wt = value, name = "py")

save(nz_population, file = "../data/nz_population.rda", compress = "bzip2")

