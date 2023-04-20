
library(readr)
library(dplyr, warn.conflicts = FALSE)

levels_time_keep <- 1980:2018
levels_age_keep <- 15:64

cn_population <- read_csv("cn_population.csv.gz",
                          col_types = "icid") %>%
    filter(age %in% levels_age_keep) %>%
    filter(time %in% levels_time_keep) %>%
    rename(py = count)

save(cn_population, file = "../data/cn_population.rda", compress = "bzip2")

