
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








                      


    
