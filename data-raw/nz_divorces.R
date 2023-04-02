
library(dplyr, warn.conflicts = FALSE)
library(poputils)

popn <- readRDS("popn.rds")
divorces <- readRDS("divorces.rds")

popn <- popn %>%
    mutate(age = set_age_open(age, lower = 65)) %>%
    count(age, sex, time, wt = count, name = "population")

divorces <- divorces %>%
    rename(divorces = value) %>%
    mutate(age = clean_age(age)) %>%
    select(age, sex, time, divorces)

nz_divorces <- inner_join(divorces, popn, by = c("age", "sex", "time")) %>%
    mutate(time = as.integer(time))

save(nz_divorces, file = "../data/nz_divorces.rda", compress = "bzip2")

