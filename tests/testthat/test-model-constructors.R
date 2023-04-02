
## 'AR1' ----------------------------------------------------------------------

test_that("'AR1' works with valid inputs", {
    ans <- AR1()
    expect_s3_class(ans, "BayesRates_model_ar1")
    ans <- AR1(coef_min = 0)
    expect_s3_class(ans, "BayesRates_model_ar1")
})

test_that("'AR1' raises correct error with invalid inputs", {
    expect_error(AR1(coef_max = 0.6),
                 "'coef_max' \\[0.6\\] is less than or equal to 'coef_min' \\[0.8\\]")
})


## 'LocalTrend' ---------------------------------------------------------------

test_that("'LocalTrend' works with valid inputs", {
    ans <- LocalTrend()
    expect_s3_class(ans, "BayesRates_model_localtrend")
    ans <- LocalTrend(scale_trend = 0.33, coef_min = 0.95)
    expect_s3_class(ans, "BayesRates_model_localtrend")
})

test_that("'LocalTrend' raises correct error with invalid inputs", {
    expect_error(LocalTrend(scale_trend = 0),
                 "'scale_trend' is less than or equal to 0")
    expect_error(LocalTrend(coef_min = 0.99),
                 "'coef_max' \\[0.98\\] is less than or equal to 'coef_min' \\[0.99\\]")
})


## 'RW2' ----------------------------------------------------------------------

test_that("'RW2' works with valid inputs", {
    ans <- RW2()
    expect_s3_class(ans, "BayesRates_model_rw2")
    ans <- RW2(scale = 0.33)
    expect_s3_class(ans, "BayesRates_model_rw2")
})

test_that("'RW2' raises correct error with invalid inputs", {
    expect_error(RW2(scale = 0),
                 "'scale' is less than or equal to 0")
})


## 'Spline' -------------------------------------------------------------------

test_that("'Spline' works with valid inputs", {
    ans <- Spline()
    expect_s3_class(ans, "BayesRates_model_spline")
    ans <- Spline(df = 3, scale = 0.33)
    expect_s3_class(ans, "BayesRates_model_spline")
})

test_that("'Spline' raises correct error with invalid inputs", {
    expect_error(Spline(df = 1))
    expect_error(Spline(scale = 0),
                 "'scale' is less than or equal to 0")
})




## 'new' ----------------------------------------------------------------------

test_that("'new_*' functions work with valid inputs", {
    expect_s3_class(new_BayesRates_model_ar1(coef_min = 0.8,
                                             coef_max = 0.9,
                                             mean = -1,
                                             sd = 2,
                                             scale = 0.5),
                    c("BayesRates_model_ar1", "BayesRates_model"))
    expect_s3_class(new_BayesRates_model_localtrend(scale_effect = 0.1,
                                                    scale_level = 0.1,
                                                    scale_trend = 0.1,
                                                    coef_min = 0.5,
                                                    coef_max = 1),
                    c("BayesRates_model_localtrend", "BayesRates_model"))
    expect_s3_class(new_BayesRates_model_rw2(scale = 0.1),
                    c("BayesRates_model_rw2", "BayesRates_model"))
    expect_s3_class(new_BayesRates_model_spline(df = 5L,
                                                scale = 0.1),
                    c("BayesRates_model_spline", "BayesRates_model"))
})
