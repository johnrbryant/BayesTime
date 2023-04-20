
library(dplyr, warn.conflicts = FALSE)

load("../data/nz_population.rda")

nz_population_2020 <- nz_population %>%
    filter(time == 2020) %>%
    select(-time)

save(nz_population_2020,
     file = "../data/nz_population_2020.rda",
     compress = "bzip2")
