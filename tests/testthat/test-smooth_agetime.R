

## 'smooth_age' -----------------------------------------------------------

test_that("'smooth_age' works with valid data - no 'by' variables", {
    set.seed(0)
    nevent_df <- tibble(age = 0:9,
                        nevent = rpois(n = 10, lambda = 11:20))
    py_df <- tibble(age = 0:9,
                    py = 100)
    ans <- smooth_age(nevent_df = nevent_df,
                      py_df = py_df)
    expect_s3_class(ans, "BayesRates_results")
})

test_that("'smooth_age' works with valid data - exposure and count both zero", {
    set.seed(0)
    nevent_df <- tibble(age = 0:9,
                        nevent = rpois(n = 10, lambda = 11:20))
    py_df <- tibble(age = 0:9,
                    py = 100)
    nevent_df$nevent[3] <- 0
    py_df$py[3] <- 0
    ans <- smooth_age(nevent_df = nevent_df,
                      py_df = py_df)
    expect_s3_class(ans, "BayesRates_results")
})

test_that("'smooth_age' throws correct error when 'agevar' starts with a .", {
    set.seed(0)
    nevent_df <- tibble(age = 0:9,
                        nevent = rpois(n = 10, lambda = 11:20))
    py_df <- tibble(.age = 0:9,
                    py = 100)
    expect_error(smooth_age(nevent_df = nevent_df,
                            py_df = py_df,
                            agevar = ".age"),
                 "'agevar' starts with a '\\.'")
})

test_that("'smooth_age' throws correct error when 'byvar' starts with a .", {
    set.seed(0)
    nevent_df <- tibble(age = rep(0:9, 2),
                        .sex = rep(c("f", "m"), each = 10),
                        nevent = rpois(n = 20, lambda = 11:30))
    py_df <- tibble(age = 0:9,
                    py = 100)
    expect_error(smooth_age(nevent_df = nevent_df,
                            py_df = py_df,
                            byvar = ".sex"),
                 "'byvar' includes a name that starts with a '\\.'")
})

test_that("'smooth_age' throws correct error when 'byvar' and 'agevar' overlap", {
    set.seed(0)
    nevent_df <- tibble(age = rep(0:9, 2),
                        sex = rep(c("f", "m"), each = 10),
                        nevent = rpois(n = 20, lambda = 11:30))
    py_df <- tibble(age = 0:9,
                    py = 100)
    expect_error(smooth_age(nevent_df = nevent_df,
                            py_df = py_df,
                            byvar = c("age", "sex")),
                 "'byvar' contains variable called \"age\"")
})


## 'smooth_agetime' -----------------------------------------------------------

test_that("'smooth_agetime' works with valid data - no 'by' variables", {
    set.seed(0)
    nevent_df <- expand.grid(age = 0:9, time = 2011:2020,
                             KEEP.OUT.ATTRS = FALSE)
    nevent_df$nevent <- rpois(n = nrow(nevent_df), lambda = outer(11:20, 5:14))
    py_df <- expand.grid(age = 0:9, time = 2011:2020,
                         KEEP.OUT.ATTRS = FALSE)
    py_df$py <- 100
    ans <- smooth_agetime(nevent_df = nevent_df,
                          py_df = py_df,
                          spec_time = TimeFixed())
    expect_s3_class(ans, "BayesRates_results")
})

test_that("'smooth_agetime' works with valid data - missing some years", {
    set.seed(0)
    nevent_df <- expand.grid(age = 0:9, time = 2011:2020,
                             KEEP.OUT.ATTRS = FALSE)
    nevent_df$nevent <- rpois(n = nrow(nevent_df), lambda = outer(11:20, 5:14))
    py_df <- expand.grid(age = 0:9, time = 2011:2020,
                         KEEP.OUT.ATTRS = FALSE)
    py_df$py <- 100
    nevent_df <- nevent_df[nevent_df$time %in% c(2011, 2015, 2020), ]
    py_df <- py_df[py_df$time %in% c(2011, 2015, 2020), ]
    ans <- smooth_agetime(nevent_df = nevent_df,
                          py_df = py_df,
                          spec_time = TimeFixed())
    expect_s3_class(ans, "BayesRates_results")
})

test_that("'smooth_agetime' throws correct error when 'agevar' starts with a .", {
  set.seed(0)
  nevent_df <- tibble(age = rep(0:9, 2),
                      time = rep(1:2, each = 10),
                      nevent = rpois(n = 20, lambda = 11:20))
  py_df <- tibble(.age = rep(0:9, 2),
                  time = rep(1:2, each = 10),
                  py = 100)
  expect_error(smooth_agetime(nevent_df = nevent_df,
                              py_df = py_df,
                              agevar = ".age"),
               "'agevar' starts with a '\\.'")
})

test_that("'smooth_agetime' throws correct error when 'timevar' starts with a .", {
  set.seed(0)
  nevent_df <- tibble(age = rep(0:9, 2),
                      .time = rep(1:2, each = 10),
                      nevent = rpois(n = 20, lambda = 11:20))
  py_df <- tibble(age = rep(0:9, 2),
                  .time = rep(1:2, each = 10),
                  py = 100)
  expect_error(smooth_agetime(nevent_df = nevent_df,
                              py_df = py_df,
                              timevar = ".time"),
               "'timevar' starts with a '\\.'")
})

test_that("'smooth_agetime' throws correct error when 'byvar' starts with a .", {
  set.seed(0)
  nevent_df <- tibble(age = rep(0:9, 2),
                      time = rep(1:2, each = 10),
                      nevent = rpois(n = 20, lambda = 11:20),
                      .sex = rep("f", 20))
  py_df <- tibble(age = rep(0:9, 2),
                  .time = rep(1:2, each = 10),
                  py = 100,
                  .sex = rep("f", 20))
  expect_error(smooth_agetime(nevent_df = nevent_df,
                              py_df = py_df,
                              byvar = ".sex"),
               "'byvar' includes a name that starts with a '\\.'")
})


