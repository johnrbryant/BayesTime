
## 'get_consts' ---------------------------------------------------------------

test_that("'get_consts' works with ar1", {
    model <- AR1()
    expect_identical(get_consts(model),
                     c(coef_min = 0.8,
                       coef_max = 0.98,
                       mean = 4,
                       sd = 2,
                       scale = 1))
})

test_that("'get_consts' works with localtrend", {
    model <- LocalTrend()
    expect_identical(get_consts(model),
                     c(scale_trend = 1,
                       scale_level = 1,
                       scale_effect = 1,
                       coef_min = 0.8,
                       coef_max = 0.98))
})

test_that("'get_consts' works with spline", {
    model <- Spline()
    expect_identical(get_consts(model),
                     c(df = 5,
                       scale = 1))
})

test_that("'get_consts' works with rw2", {
    model <- RW2()
    expect_identical(get_consts(model),
                     c(scale = 1))
})


## 'get_par' ------------------------------------------------------------------

test_that("'get_par' works with ar1", {
    model <- AR1()
    labels <- c("0-4", "5-9", "10-14")
    expect_identical(get_par(model, labels = labels),
                     c(logit_phi = log(0.89 / 0.11),
                       alpha = 4,
                       log_sd = 0,
                       "effect.0-4" = 0, "effect.5-9" = 0, "effect.10-14" = 0))
})

test_that("'get_par' works with localtrend", {
    model <- LocalTrend()
    labels <- c("0-4", "5-9", "10-14")
    expect_identical(get_par(model, labels = labels),
                     c(log_sd_trend = 0,
                       log_sd_level = 0,
                       log_sd_effect = 0,
                       logit_phi = log(0.89 / 0.11),
                       "trend.0-4" = 0, "trend.5-9" = 0,
                       "level.5-9" = 0, "level.10-14" = 0,
                       "effect.0-4" = 0, "effect.5-9" = 0, "effect.10-14" = 0))
})

test_that("'get_par' works with spline", {
    model <- Spline()
    labels <- c("0-4", "5-9", "10-14")
    expect_identical(get_par(model, labels),
                     c(log_sd = 0,
                       freepar.1 = 0))
})

test_that("'get_par' works with rw2", {
    model <- RW2()
    labels <- c("0-4", "5-9", "10-14")
    expect_identical(get_par(model, labels),
                     c(log_sd = 0,
                       freepar.1 = 0))
})


## 'get_transforms_hyper' -----------------------------------------------------

test_that("'get_transforms_hyper' works with ar1", {
    model <- AR1()
    labels <- c("0-4", "5-9", "10-14")
    invlogit <- function(x) exp(x) / (1 + exp(x))
    ans <- get_transforms_hyper(model, labels = labels)
    expect_identical(names(ans), c("coef", "average", "sd"))
    expect_equal(ans[[1]](0.3), invlogit(0.3) * 0.18 + 0.8)
    expect_equal(ans[[2]](3), 3)
    expect_equal(ans[[3]](0.2), exp(0.2))
})

test_that("'get_transforms_hyper' works with localtrend", {
    model <- LocalTrend()
    labels <- c("0-4", "5-9", "10-14")
    invlogit <- function(x) exp(x) / (1 + exp(x))
    ans <- get_transforms_hyper(model, labels = labels)
    expect_identical(names(ans), c("sd_trend", "sd_level", "sd_effect",
                                   "coef",
                                   "trend.0-4", "trend.5-9",
                                   "level.5-9", "level.10-14"))
    for (i in 1:3)
        expect_equal(ans[[i]](0.2), exp(0.2))
    expect_equal(ans[[4]](0.3), invlogit(0.3) * 0.18 + 0.8)
    for (i in 5:8)
        expect_equal(ans[[i]](0.93), 0.93)
})

test_that("'get_transforms_hyper' works with spline", {
    model <- Spline()
    labels <- c("0-4", "5-9", "10-14")
    ans <- get_transforms_hyper(model, labels = labels)
    expect_identical(names(ans), "sd")
    expect_equal(ans[[1]](0.2), exp(0.2))
})

test_that("'get_transforms_hyper' works with rw2", {
    model <- RW2()
    labels <- c("0-4", "5-9", "10-14")
    ans <- get_transforms_hyper(model, labels = labels)
    expect_identical(names(ans), "sd")
    expect_equal(ans[[1]](0.2), exp(0.2))
})


## 'get_X_age' ----------------------------------------------------------------

test_that("'get_X_age' works with rw2", {
    labels_age <- c("0-4", "5-9", "10-14")
    model <- RW2()
    ans_obtained <- get_X_age(model, labels_age = labels_age)
    ans_expected <- make_rw2_matrix(n = 3)
    colnames(ans_expected) <- 1
    rownames(ans_expected) <- labels_age
    names(dimnames(ans_expected)) <- c("age", "freepar")
    expect_identical(ans_obtained, ans_expected)                     
})










