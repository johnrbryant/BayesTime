
## 'check_age_min_supplied' ---------------------------------------------------

test_that("'check_age_min_supplied' works with character/factor age variable", {
    expect_true(check_age_min_supplied(has_age_min = TRUE,
                                       agevar_val = "a"))
    expect_true(check_age_min_supplied(has_age_min = TRUE,
                                       agevar_val = factor("a")))
    expect_error(check_age_min_supplied(has_age_min = FALSE,
                                        agevar_val = "a"),
                 "'nevent_df' uses non-integer age labels, but no value supplied for 'age_min'")
    expect_error(check_age_min_supplied(has_age_min = FALSE,
                                        agevar_val = factor("a")),
                 "'nevent_df' uses non-integer age labels, but no value supplied for 'age_min'")
})

test_that("'check_age_min_supplied' works with integer age variable", {
    expect_error(check_age_min_supplied(has_age_min = TRUE,
                                        agevar_val = 1L),
                 "'nevent_df' uses integer age labels, but value supplied for 'age_min'")
    expect_true(check_age_min_supplied(has_age_min = FALSE,
                                            agevar_val = 1L))
})


## 'check_age_width_df' -------------------------------------------------------

test_that("'check_age_width_df' returns TRUE with valid value", {
    expect_true(check_age_width_df(age_width_df = data.frame(age = 0:2,
                                                       width = rep(1, 3)),
                                agevar_val = 2:1))
    expect_true(check_age_width_df(age_width_df = data.frame(age = c("0-4", "5-9", "10+"),
                                                       width = rep(5, 3)),
                                agevar_val = c("5-9", "5-9", "10+")))
})

test_that("'check_age_width_df' raises correct error with invalid col names", {
    expect_error(check_age_width_df(age_width_df = data.frame(age = 0:2,
                                                       wrong = rep(1, 3)),
                                 agevar_val = 2:1),
                 "colnames for 'age_width_df' invalid : should be \"age\", \"width\"")
})

test_that("'check_age_width_df' raises correct error with invalid age column", {
    expect_error(check_age_width_df(age_width_df = data.frame(age = 2:4,
                                                        width = rep(1, 3)),
                                 agevar_val = 2:1),
                 "variable 'age' in 'age_width_df' does not include age group \"1\"")
})

test_that("'check_age_width_df' raises correct error with invalid age column", {
    expect_error(check_age_width_df(age_width_df = data.frame(age = 2:4,
                                                        width = c(1, 1, Inf)),
                                 agevar_val = 2:4),
                 "problem with variable 'width' in 'age_width_df' :")
})


## 'check_age_width_df_supplied' ----------------------------------------------

test_that("'check_age_width_df_supplied' works with character/factor age variable", {
    expect_true(check_age_width_df_supplied(has_age_width_df = TRUE,
                                            agevar_val = "a"))
    expect_true(check_age_width_df_supplied(has_age_width_df = TRUE,
                                            agevar_val = factor("a")))
    expect_error(check_age_width_df_supplied(has_age_width_df = FALSE,
                                             agevar_val = "a"),
                 "'nevent_df' uses non-integer age labels, but no value supplied for 'age_width_df'")
    expect_error(check_age_width_df_supplied(has_age_width_df = FALSE,
                                             agevar_val = factor("a")),
                 "'nevent_df' uses non-integer age labels, but no value supplied for 'age_width_df'")
})

test_that("'check_age_width_df_supplied' works with integer age variable", {
    expect_true(check_age_width_df_supplied(has_age_width_df = TRUE,
                                            agevar_val = 1L))
    expect_true(check_age_width_df_supplied(has_age_width_df = FALSE,
                                            agevar_val = 1L))
})


## 'check_all_combn_classif_vars' ---------------------------------------------

test_that("'check_all_combn_classif_vars' returns TRUE with valid value", {
    df <- data.frame(age = rep(0:1, 2), sex = c("f", "f", "m", "m"))
    expect_true(check_all_combn_classif_vars(df = df,
                                             nm_df = "data",
                                             nms_classif_vars = c("sex", "age")))
})

test_that("'check_all_combn_classif_vars' returns expected error with one row missing", {
    df <- data.frame(age = c(0, 1, 0), sex = c("f", "f", "m"))
    expect_error(check_all_combn_classif_vars(df = df,
                                              nm_df = "data",
                                              nms_classif_vars = c("sex", "age")),
                 paste0("'data' missing combination of classification variables:\n",
                        "    sex: m\n",
                        "    age: 1"))                       
})

test_that("'check_all_combn_classif_vars' returns expected error with two rows missing", {
    df <- data.frame(age = c(0, 1), sex = c("f", "m"))
    expect_error(check_all_combn_classif_vars(df = df,
                                              nm_df = "data",
                                              nms_classif_vars = c("age", "sex")),
                 paste0("'data' missing combination of classification variables:\n",
                        "    age: 1\n",
                        "    sex: f"))
})


## 'check_df_same_levels' -----------------------------------------------------

test_that("'check_df_same_levels' returns TRUE with valid inputs", {
    classif_vars <- expand.grid(age = c(0, 1), sex = c("f", "m"), time = 2001:2002,
                                KEEP.OUT.ATTRS = FALSE)
    nevent_df <- classif_vars
    py_df <- classif_vars[8:1,]
    nevent_df$nevent <- 10
    py_df$py <- 20
    expect_true(check_df_same_levels(nevent_df = nevent_df, py_df = py_df))
})

test_that("'check_df_same_levels' returns correct error with invalid inputs", {
    classif_vars <- expand.grid(age = c(0, 1), sex = c("f", "m"), time = 2001:2002,
                                KEEP.OUT.ATTRS = FALSE)
    nevent_df <- classif_vars
    py_df <- classif_vars[1:4,]
    nevent_df$nevent <- 10
    py_df$py <- 20
    expect_error(check_df_same_levels(nevent_df = nevent_df, py_df = py_df),
                 "variable 'time' in 'nevent_df' uses different categories from variable 'time' in 'py_df'")
})


## 'check_df_zero_na' ---------------------------------------------------------

test_that("'check_df_zero_na' returns TRUE with valid inputs", {
    classif_vars <- expand.grid(age = c(0, 1), sex = c("f", "m"), time = 2001:2002,
                                KEEP.OUT.ATTRS = FALSE)
    df <- classif_vars
    df$nevent <- c(0, NA, 0,  NA, rep(1, 4))
    df$py <-     c(0, 0,  NA, NA, rep(2, 4))
    expect_true(check_df_zero_na(df))
})


test_that("'check_df_zero_na' returns correct error with invalid inputs", {
    classif_vars <- expand.grid(age = c(0, 1), sex = c("f", "m"), time = 2001:2002,
                                KEEP.OUT.ATTRS = FALSE)
    df <- classif_vars
    df <- classif_vars
    df$nevent <- c(0, NA, 0,  1, rep(1, 4))
    df$py <-     c(0, 0,  NA, 0, rep(2, 4))
    expect_error(check_df_zero_na(df),
                 paste0("invalid combination of values for 'nevent' and 'py' :\n",
                        "  age = 1\n",
                        "  sex = m\n",
                        "  time = 2001\n",
                        "  nevent = 1\n",
                        "  py = 0"))
})


## 'check_gt_zero' ------------------------------------------------------------

test_that("'check_gt_zero' returns TRUE with valid value", {
    expect_true(check_gt_zero(0.01, nm = "x"))
    expect_true(check_gt_zero(Inf, nm = "x"))
})

test_that("'check_gt_zero' returns TRUE with valid value", {
    expect_error(check_gt_zero(0, nm = "x"),
                 "'x' is less than or equal to 0")
    expect_error(check_gt_zero(-3, nm = "y"),
                 "'y' is less than or equal to 0")
})


## 'check_input_notime_df' ----------------------------------------------------

test_that("'check_input_notime_df' returns TRUE with valid value - no duplicates", {
    df <- expand.grid(age = 0:2, sex = c("F", "M"),
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 3
    expect_true(check_input_notime_df(df = df,
                                      measurevar = "count",
                                      agevar = "age",
                                      byvar = "sex"))
})

test_that("'check_input_notime_df' returns TRUE with valid value - with duplicates", {
    df <- expand.grid(age = 0:2, sex = c("F", "M"),
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 3
    expect_true(check_input_notime_df(df = df,
                                      measurevar = "count",
                                      agevar = "age",
                                      byvar = character()))
})

test_that("'check_input_notime_df' throws correct error when variable missing", {
    df <- expand.grid(age = c(0, 1, 2.5), sex = c("F", "M"),
                      KEEP.OUT.ATTRS = FALSE)
    df$wrong <- 3
    expect_error(check_input_notime_df(df = df,
                                       measurevar = "count",
                                       agevar = "age",
                                       byvar = character()),
                 "data frame 'count_df' does not have a variable called \"count\"")
})

test_that("'check_input_notime_df' throws correct error when age var numeric with fractions", {
    df <- expand.grid(age = c(0, 1, 2.5), sex = c("F", "M"),
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 3
    expect_error(check_input_notime_df(df = df,
                                       measurevar = "count",
                                       agevar = "age",
                                       byvar = character()),
                 "problem with variable 'age' in data frame 'count_df' :")
})

test_that("'check_input_notime_df' throws correct error when age var numeric with fractions", {
    df <- expand.grid(age = c(0, 1, 3), sex = c("F", "M"),
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 3
    expect_error(check_input_notime_df(df = df,
                                       measurevar = "count",
                                       agevar = "age",
                                       byvar = character()),
                 "variable 'age' in data frame 'count_df' does not have a complete set of levels")
})

test_that("'check_input_notime_df' throws correct error when non-numeric age var has NAs", {
    df <- expand.grid(age = factor(c(0, 1, NA)), sex = c("F", "M"),
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 3
    expect_error(check_input_notime_df(df = df,
                                       measurevar = "count",
                                       agevar = "age",
                                       byvar = character()),
                 "variable 'age' in data frame 'count_df' has NAs")
})

test_that("'check_input_notime_df' throws correct error when age is list", {
    df <- expand.grid(age = factor(c(0, 1, NA)), sex = c("F", "M"),
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 3
    df$age <- rep(list(1:3), times = 2)
    expect_error(check_input_notime_df(df = df,
                                       measurevar = "count",
                                       agevar = "age",
                                       byvar = character()),
                 "variable 'age' in data frame 'count_df' has class \"list\"")
})

test_that("'check_input_notime_df' throws correct error when measure var negative", {
    df <- expand.grid(age = 0:2, sex = c("F", "M"),
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- -1
    expect_error(check_input_notime_df(df = df,
                                       measurevar = "count",
                                       agevar = "age",
                                       byvar = character()),
                 "problem with variable 'count' in data frame 'count_df' :")
})

test_that("'check_input_notime_df' warns when df appears to have timevar", {
    df <- expand.grid(age = 0:2, sex = c("F", "M"), period = 2001:2002,
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 1
    expect_warning(check_input_notime_df(df = df,
                                         measurevar = "count",
                                         agevar = "age",
                                         byvar = character()),
                   "'count_df' appears to include a time variable \\[\"period\"\\]")
})

test_that("'check_input_notime_df' throws correct error when less than 3 age values", {
    df <- expand.grid(age = 0:1, sex = c("F", "M"),
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 1
    expect_error(check_input_notime_df(df = df,
                                       measurevar = "count",
                                       agevar = "age",
                                       byvar = character()),
                 "age variable \\['age'\\] in data frame 'count_df' has only 2 unique value\\(s\\) : needs at least 3")
})



## 'check_input_withtime_df' --------------------------------------------------

test_that("'check_input_withtime_df' returns TRUE with valid value - no duplicates", {
    df <- expand.grid(age = 0:2, sex = c("F", "M"), time = 2000:2001,
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 3
    expect_true(check_input_withtime_df(df = df,
                               measurevar = "count",
                               agevar = "age",
                               timevar = "time",
                               byvar = "sex"))
})

test_that("'check_input_withtime_df' returns TRUE with valid value - with duplicates", {
    df <- expand.grid(age = 0:2, sex = c("F", "M"), time = 2000:2001,
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 3
    expect_true(check_input_withtime_df(df = df,
                               measurevar = "count",
                               agevar = "age",
                               timevar = "time",
                               byvar = character()))
})

test_that("'check_input_withtime_df' throws correct error when variable missing", {
    df <- expand.grid(age = c(0, 1, 2.5), sex = c("F", "M"), time = 2000:2001,
                      KEEP.OUT.ATTRS = FALSE)
    df$wrong <- 3
    expect_error(check_input_withtime_df(df = df,
                               measurevar = "count",
                               agevar = "age",
                               timevar = "time",
                               byvar = character()),
                 "data frame 'count_df' does not have a variable called \"count\"")
})

test_that("'check_input_withtime_df' throws correct error when age var numeric with fractions", {
    df <- expand.grid(age = c(0, 1, 2.5), sex = c("F", "M"), time = 2000:2001,
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 3
    expect_error(check_input_withtime_df(df = df,
                               measurevar = "count",
                               agevar = "age",
                               timevar = "time",
                               byvar = character()),
                 "problem with variable 'age' in data frame 'count_df' :")
})

test_that("'check_input_withtime_df' throws correct error when age var numeric with fractions", {
    df <- expand.grid(age = c(0, 1, 3), sex = c("F", "M"), time = 2000:2001,
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 3
    expect_error(check_input_withtime_df(df = df,
                               measurevar = "count",
                               agevar = "age",
                               timevar = "time",
                               byvar = character()),
                 "variable 'age' in data frame 'count_df' does not have a complete set of levels")
})

test_that("'check_input_withtime_df' throws correct error when non-numeric age var has NAs", {
    df <- expand.grid(age = factor(c(0, 1, NA)), sex = c("F", "M"), time = 2000:2001,
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 3
    expect_error(check_input_withtime_df(df = df,
                               measurevar = "count",
                               agevar = "age",
                               timevar = "time",
                               byvar = character()),
                 "variable 'age' in data frame 'count_df' has NAs")
})

test_that("'check_input_withtime_df' throws correct error when age is list", {
    df <- expand.grid(age = factor(c(0, 1, NA)), sex = c("F", "M"), time = 2000:2001,
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 3
    df$age <- rep(list(1:3), times = 4)
    expect_error(check_input_withtime_df(df = df,
                               measurevar = "count",
                               agevar = "age",
                               timevar = "time",
                               byvar = character()),
                 "variable 'age' in data frame 'count_df' has class \"list\"")
})

test_that("'check_input_withtime_df' throws correct error when time var not integerish", {
    df <- expand.grid(age = 0:2, sex = c("F", "M"), time = c(2000, 2001.5),
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 3
    expect_error(check_input_withtime_df(df = df,
                               measurevar = "count",
                               agevar = "age",
                               timevar = "time",
                               byvar = character()),
                 "problem with variable 'time' in data frame 'count_df' :")
})

test_that("'check_input_withtime_df' throws correct error when measure var negative", {
    df <- expand.grid(age = 0:2, sex = c("F", "M"), time = 2000:2001,
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- -1
    expect_error(check_input_withtime_df(df = df,
                               measurevar = "count",
                               agevar = "age",
                               timevar = "time",
                               byvar = character()),
                 "problem with variable 'count' in data frame 'count_df' :")
})


test_that("'check_input_withtime_df' throws correct error when less than 3 age values", {
    df <- expand.grid(age = 0:1, sex = c("F", "M"), time = 2000:2001,
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 1
    expect_error(check_input_withtime_df(df = df,
                                         measurevar = "count",
                                         agevar = "age",
                                         timevar = "time",
                                         byvar = character()),
                 "age variable \\['age'\\] in data frame 'count_df' has only 2 unique value\\(s\\) : needs at least 3")
})


test_that("'check_input_withtime_df' throws correct error when less than 2 time values", {
    df <- expand.grid(age = 0:2, sex = c("F", "M"), time = 2000,
                      KEEP.OUT.ATTRS = FALSE)
    df$count <- 1
    expect_error(check_input_withtime_df(df = df,
                                         measurevar = "count",
                                         agevar = "age",
                                         timevar = "time",
                                         byvar = character()),
                 "time variable \\['time'\\] in data frame 'count_df' has only 1 unique value\\(s\\) : needs at least 2")
})









                      


    
