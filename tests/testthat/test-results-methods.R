

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
    expect_true(all(c(".fitted", ".lower", ".upper", ".observed") %in% names(ans)))
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
                    c("age", ".fitted", ".lower", ".upper"))
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
                    c("age", ".fitted", ".lower", ".upper"))
    expect_setequal(names(ans$age_hyper),
                    c("hyper", ".fitted", ".lower", ".upper"))
})


## 'total_rates' --------------------------------------------------------------

test_that("'total_rates' works with valid inputs - smooth_age", {
    set.seed(0)
    nevent_df <- tibble(age = 0:9,
                        nevent = rpois(n = 10, lambda = 11:20))
    py_df <- tibble(age = 0:9,
                    py = 100)
    results <- smooth_age(nevent_df = nevent_df,
                          py_df = py_df)
    ans <- total_rates(results)
    expect_setequal(names(ans),
                    c(".fitted", ".lower", ".upper", ".observed"))
    rates <- components(results)
    expect_equal(sum(rates$.fitted), ans$.fitted, tolerance = 0.01)
})

test_that("'total_rates' works with valid inputs - smooth_agetime", {
    set.seed(0)
    nevent_df <- expand.grid(age = 0:9, time = 2011:2013,
                             KEEP.OUT.ATTRS = FALSE)
    nevent_df$nevent <- rpois(n = nrow(nevent_df), lambda = outer(11:20, 5:7))
    py_df <- expand.grid(age = 0:9, time = 2011:2013,
                         KEEP.OUT.ATTRS = FALSE)
    py_df$py <- 100
    results <- smooth_agetime(nevent_df = nevent_df,
                              py_df = py_df)
    ans <- total_rates(results, n_draw = 20)
    expect_setequal(names(ans), c("time", ".fitted", ".lower", ".upper", ".observed"))
    rates <- components(results, n_draw = 20)
    expect_equal(sum(rates$.fitted), sum(ans$.fitted), tolerance = 0.01)
})

test_that("'total_rates' works with valid inputs - character age groups", {
    set.seed(0)
    nevent_df <- tibble(age = c("0-4", "5-9", "10+"),
                        nevent = rpois(n = 3, lambda = 11:13))
    py_df <- tibble(age = c("0-4", "5-9", "10+"),
                    py = 100)
    results <- smooth_age(nevent_df = nevent_df,
                          py_df = py_df)
    age_width_df <- data.frame(age = c("0-4", "5-9", "10+"),
                               width = c(5, 5, 10))
    ans <- total_rates(results, age_width_df = age_width_df)
    expect_setequal(names(ans),
                    c(".fitted", ".lower", ".upper", ".observed"))
    rates <- components(results)
    expect_equal(sum(rates$.fitted * age_width_df$width), ans$.fitted, tolerance = 0.02)
})

