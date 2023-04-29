

## 'augment' ------------------------------------------------------------------

test_that("'augment' works", {
    set.seed(0)
    nevent_df <- expand.grid(age = 0:9, time = 2011:2020,
                             KEEP.OUT.ATTRS = FALSE)
    nevent_df$nevent <- rpois(n = nrow(nevent_df), lambda = outer(11:20, 5:14))
    py_df <- expand.grid(age = 0:9, time = 2011:2020,
                         KEEP.OUT.ATTRS = FALSE)
    py_df$py <- 100
    results <- smooth_agetime(nevent_df = nevent_df,
                              py_df = py_df)
    ans <- augment(results, n_draw = 5)
    expect_true(all(c(".fitted", ".lower", ".upper", ".probability",
                      ".observed") %in% names(ans)))
})


## 'components' ---------------------------------------------------------------

test_that("'components' works with valid inputs - rates", {
    set.seed(0)
    nevent_df <- tibble(age = 0:9,
                        nevent = rpois(n = 10, lambda = 11:20))
    py_df <- tibble(age = 0:9,
                    py = 100)
    results <- smooth_age(nevent_df = nevent_df,
                          py_df = py_df)
    ans <- components(results, "rates", n_draw = 5)
    expect_setequal(names(ans),
                    c("age", "age.mid",
                      ".fitted", ".lower", ".upper", ".probability"))
})

test_that("'components' works with valid inputs - age effect, age hyper", {
    set.seed(0)
    nevent_df <- tibble(age = 0:9,
                        nevent = rpois(n = 10, lambda = 11:20))
    py_df <- tibble(age = 0:9,
                    py = 100)
    results <- smooth_age(nevent_df = nevent_df,
                          py_df = py_df)
    ans <- components(results, what = c("age_effect", "age_hyper"), n_draw = 5)
    expect_identical(names(ans), c("age_effect", "age_hyper"))
    expect_setequal(names(ans$age_effect),
                    c("age", "age.mid",
                      ".fitted", ".lower", ".upper", ".probability"))
    expect_setequal(names(ans$age_hyper),
                    c("hyper", ".fitted", ".lower", ".upper", ".probability"))
})


## 'n_draw' -------------------------------------------------------------------

test_that("'n_draw' works", {
    set.seed(0)
    nevent_df <- expand.grid(age = 0:9, time = 2011:2020,
                             KEEP.OUT.ATTRS = FALSE)
    nevent_df$nevent <- rpois(n = nrow(nevent_df), lambda = outer(11:20, 5:14))
    py_df <- expand.grid(age = 0:9, time = 2011:2020,
                         KEEP.OUT.ATTRS = FALSE)
    py_df$py <- 100
    results <- smooth_agetime(nevent_df = nevent_df,
                              py_df = py_df)
    expect_identical(results$n_draw, 1000L)
    n_draw(results) <- 1L
    expect_identical(results$n_draw, 1L)
})


## 'total_rate' --------------------------------------------------------------

test_that("'total_rate' works with valid inputs - smooth_age", {
    set.seed(0)
    nevent_df <- tibble(age = 0:9,
                        nevent = rpois(n = 10, lambda = 11:20))
    py_df <- tibble(age = 0:9,
                    py = 100)
    results <- smooth_age(nevent_df = nevent_df,
                          py_df = py_df)
    ans <- total_rate(results)
    expect_setequal(names(ans),
                    c(".fitted", ".lower", ".upper", ".probability", ".observed"))
    rates <- components(results)
    expect_equal(sum(rates$.fitted), ans$.fitted, tolerance = 0.01)
})

test_that("'total_rate' works with valid inputs - smooth_agetime", {
    set.seed(0)
    nevent_df <- expand.grid(age = 0:9, time = 2011:2013,
                             KEEP.OUT.ATTRS = FALSE)
    nevent_df$nevent <- rpois(n = nrow(nevent_df), lambda = outer(11:20, 5:7))
    py_df <- expand.grid(age = 0:9, time = 2011:2013,
                         KEEP.OUT.ATTRS = FALSE)
    py_df$py <- 100
    results <- smooth_agetime(nevent_df = nevent_df,
                              py_df = py_df,
                              spec_age = Spline(df = 4, scale =2),
                              spec_time = TimeVarying(scale = 2))
    n_draw(results) <- 20
    ans <- total_rate(results)
    expect_setequal(names(ans),
                    c("time", ".fitted", ".lower", ".upper",
                      ".probability", ".observed"))
    rates <- components(results)
    expect_equal(sum(rates$.fitted), sum(ans$.fitted), tolerance = 0.01)
})

test_that("'total_rate' works with valid inputs - character age groups", {
    set.seed(0)
    nevent_df <- tibble(age = c("0-4", "5-9", "10+"),
                        nevent = rpois(n = 3, lambda = 11:13))
    py_df <- tibble(age = c("0-4", "5-9", "10+"),
                    py = 100)
    age_width_df <- data.frame(age = c("0-4", "5-9", "10+"),
                               width = c(5, 5, 10))
    results <- smooth_age(nevent_df = nevent_df,
                          py_df = py_df,
                          age_width_df = age_width_df,
                          age_min = 0)
    ans <- total_rate(results)
    expect_setequal(names(ans),
                    c(".fitted", ".lower", ".upper", ".probability", ".observed"))
    rates <- components(results)
    expect_equal(sum(rates$.fitted * age_width_df$width), ans$.fitted, tolerance = 0.02)
})

test_that("'total_rate' interpolates correctly", {
    set.seed(0)
    nevent_df <- expand.grid(age = 0:9, time = c(2011, 2013),
                             KEEP.OUT.ATTRS = FALSE)
    nevent_df$nevent <- rpois(n = nrow(nevent_df), lambda = outer(11:20, c(5, 7)))
    py_df <- expand.grid(age = 0:9, time = c(2011, 2013),
                         KEEP.OUT.ATTRS = FALSE)
    py_df$py <- 100
    results <- smooth_agetime(nevent_df = nevent_df,
                              py_df = py_df,
                              spec_age = Spline(df = 4, scale =2),
                              spec_time = TimeVarying(scale = 2))
    n_draw(results) <- 20
    ans <- total_rate(results)
    expect_setequal(names(ans),
                    c("time", ".fitted", ".lower", ".upper",
                      ".probability", ".observed"))
    expect_true(2012 %in% ans$time)
    rates <- components(results)
    expect_equal(sum(rates$.fitted), sum(ans$.fitted), tolerance = 0.02)
})


