
library(readr)
library(dplyr, warn.conflicts = FALSE)

levels_time_keep <- 1980:2018
levels_age_keep <- 15:64

cn_divorces <- read_csv("cn_divorces.csv.gz",
                        col_types = "icii") %>%
    filter(age %in% levels_age_keep) %>%
    filter(time %in% levels_time_keep) %>%
    rename(nevent = count)

save(cn_divorces, file = "../data/cn_divorces.rda", compress = "bzip2")

