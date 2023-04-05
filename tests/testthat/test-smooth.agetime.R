

## 'smooth.agetime' -----------------------------------------------------------

test_that("'smooth.agetime' works with valid data - no 'by' variables", {
    set.seed(0)
    nevent_df <- expand.grid(age = 0:9, time = 2011:2020,
                             KEEP.OUT.ATTRS = FALSE)
    nevent_df$nevent <- rpois(n = nrow(nevent_df), lambda = outer(11:20, 5:14))
    py_df <- expand.grid(age = 0:9, time = 2011:2020,
                         KEEP.OUT.ATTRS = FALSE)
    py_df$py <- 10
    ans <- smooth.agetime(nevent_df = nevent_df,
                          py_df = py_df,
                          n_draw = 10)
    expect_s3_class(ans, "BayesRates_results")
})



## 'reformat_array' -----------------------------------------------------------

test_that("'reformat_array' works with valid input", {
    x <- matrix(1:4, nr = 2, dimnames = list(draw = 1:2, time = 2001:2002))
    ans_obtained <- reformat_array(x)
    ans_expected <- tibble(draw = c(1:2, 1:2),
                           time = as.character(c(2001, 2001, 2002, 2002)),
                           value = 1:4)
    expect_identical(ans_obtained, ans_expected)
})


## 'reformat_age_effect' ------------------------------------------------------

test_that("'reformat_age_effect' works with valid input", {
    x <- matrix(1:4,
                nr = 2,
                dimnames = list(draw = 1:2, age = 0:1))
    df <- data.frame(age = factor(c("0", "1", "2")))
    ans_obtained <- reformat_age_effect(x, df = df, agevar = "age")
    ans_expected <- tibble(draw = rep(1:2, times = 2),
                           age = factor(c("0", "0", "1", "1"), levels = 0:2),
                           value = 1:4)
    expect_identical(ans_obtained, ans_expected)
})


## 'reformat_intercept' -------------------------------------------------------

test_that("'reformat_intercept' works with valid input", {
    x <- matrix(11:14,
                nr = 4,
                dimnames = list(draw = 1:4,  "(Intercept)" = "(Intercept)"))
    ans_obtained <- reformat_intercept(x)
    ans_expected <- tibble(draw = 1:4,
                           value = 11:14)
    expect_identical(ans_obtained, ans_expected)
})


## 'reformat_rates' -----------------------------------------------------------

test_that("'reformat_rates' works with valid input", {
    x <- array(1:8,
               dim = c(2, 2, 2),
               dimnames = list(draw = 1:2, age = 0:1, time = 2001:2002))
    df <- data.frame(age = factor(c("0", "1", "2")),
                     time = c(2002, 2001, 2000))
    ans_obtained <- reformat_rates(x, df = df, agevar = "age", timevar = "time")
    ans_expected <- tibble(draw = rep(1:2, times = 4),
                           age = factor(rep(c("0", "0", "1", "1"), times = 2), levels = 0:2),
                           time = rep(c(2001, 2002), each = 4),
                           value = 1:8)
    expect_identical(ans_obtained, ans_expected)
})


## 'reformat_time_effect' -----------------------------------------------------

test_that("'reformat_time_effect' works with valid input", {
    x <- matrix(1:4,
                nr = 2,
                dimnames = list(draw = 1:2, time = 0:1))
    df <- data.frame(time = c(1, 0))
    ans_obtained <- reformat_time_effect(x, df = df, timevar = "time")
    ans_expected <- tibble(draw = rep(1:2, times = 2),
                           time = c(0, 0, 1, 1),
                           value = 1:4)
    expect_identical(ans_obtained, ans_expected)
})


## 'reformat_var_age' ---------------------------------------------------------

test_that("'reformat_var_age' works with valid input", {
    expect_identical(reformat_var_age(var_current = c(0, 1),
                                      var_target = 0:1,
                                      agevar = "Age"),
                     0:1)
    expect_identical(reformat_var_age(var_current = c(0, 1),
                                      var_target = c(1, 0, 2),
                                      agevar = "Age"),
                     c(0, 1))
    expect_identical(reformat_var_age(var_current = c(0, 1),
                                      var_target = factor(c(1, 0, 2), levels = 2:0),
                                      agevar = "Age"),
                     factor(c(0, 1), levels = 2:0))
    expect_identical(reformat_var_age(var_current = 0:1,
                                      var_target = "2",
                                      agevar = "Age"),
                     c("0", "1"))
})

test_that("'reformat_var_age' throws correct error with invalid input", {
    expect_error(reformat_var_age(var_current = 0:1,
                                  var_target = NULL,
                                  agevar = "Age"),
                 "'Age' has class \"NULL\"")
})


## 'reformat_var_time' ---------------------------------------------------------

test_that("'reformat_var_time' works with valid input", {
    expect_identical(reformat_var_time(var_current = c(0, 1),
                                       var_target = 0:1,
                                       timevar = "Time"),
                     0:1)
    expect_identical(reformat_var_time(var_current = c(0, 1),
                                       var_target = c(1, 0, 2),
                                       timevar = "Time"),
                     c(0, 1))
})

test_that("'reformat_var_time' throws correct error with invalid input", {
    expect_error(reformat_var_time(var_current = 0:1,
                                   var_target = "a",
                                  timevar = "Time"),
                 "'Time' has class \"character\"")
})


    
    


