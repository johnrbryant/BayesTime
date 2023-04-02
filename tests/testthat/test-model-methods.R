
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
                     c(scale_effect = 1,
                       scale_level = 1,
                       scale_trend = 1,
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
    expect_identical(get_par(model, n_effect = 3),
                     c(logit_phi = log(0.89 / 0.11),
                       alpha = 4,
                       log_sd = 0,
                       effect_1 = 0, effect_2 = 0, effect_3 = 0))
})

test_that("'get_par' works with localtrend", {
    model <- LocalTrend()
    expect_identical(get_par(model, n_effect = 3),
                     c(log_sd_effect = 0,
                       log_sd_level = 0,
                       log_sd_trend = 0,
                       logit_phi = log(0.89 / 0.11),
                       effect_1 = 0, effect_2 = 0, effect_3 = 0,
                       level_1 = 0, level_2 = 0, level_3 = 0,
                       trend_1 = 0, trend_2 = 0))
})

test_that("'get_par' works with spline", {
    model <- Spline()
    expect_identical(get_par(model, n_effect = 3),
                     c(log_sd = 0,
                       coef_1 = 0))
})

test_that("'get_par' works with rw2", {
    model <- RW2()
    expect_identical(get_par(model, n_effect = 3),
                     c(log_sd = 0,
                       coef_1 = 0))
})


## 'get_X_age' ----------------------------------------------------------------

test_that("'get_X_age' works with rw2", {
    model <- RW2()
    expect_identical(get_X_age(model, n_age = 3),
                     make_rw2_matrix(n = 3))
})










