
library(dplyr, warn.conflicts = FALSE)

load("../data/nz_divorces.rda")

nz_divorces_2020 <- nz_divorces %>%
    filter(time == 2020) %>%
    select(-time)

save(nz_divorces_2020,
     file = "../data/nz_divorces_2020.rda",
     compress = "bzip2")

