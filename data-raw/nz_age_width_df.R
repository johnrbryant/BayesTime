
library(dplyr, warn.conflicts = FALSE)

load("../data/nz_divorces_2020.rda")

nz_age_width_df <- nz_divorces_2020 %>%
    select(age) %>%
    unique() %>%
    mutate(width = if_else(age == "65+", 15, 5))

save(nz_age_width_df,
     file = "../data/nz_age_width_df.rda",
     compress = "bzip2")

