
## 'TimeFixed' ----------------------------------------------------------------

test_that("'TimeFixed' works with valid inputs", {
    ans <- TimeFixed()
    expect_s3_class(ans, "BayesRates_spec_timefixed")
    ans <- TimeFixed(scale = 2)
    expect_s3_class(ans, "BayesRates_spec_timefixed")
})

test_that("'TimeFixed' raises correct error with invalid inputs", {
    expect_error(TimeFixed(scale = 0),
                 "'scale' is less than or equal to 0")
})


## 'TimeVarying' --------------------------------------------------------------

test_that("'TimeVarying' works with valid inputs", {
    ans <- TimeVarying()
    expect_s3_class(ans, "BayesRates_spec_timevarying")
    ans <- TimeVarying(scale = 0.33)
    expect_s3_class(ans, "BayesRates_spec_timevarying")
})

test_that("'TimeVarying' raises error with invalid inputs", {
    expect_error(TimeVarying(scale = 0),
                 "'scale' is less than or equal to 0")
})


## 'RW2' ----------------------------------------------------------------------

test_that("'RW2' works with valid inputs", {
    ans <- RW2()
    expect_s3_class(ans, "BayesRates_spec_rw2")
    ans <- RW2(scale = 0.33)
    expect_s3_class(ans, "BayesRates_spec_rw2")
})

test_that("'RW2' raises correct error with invalid inputs", {
    expect_error(RW2(scale = 0),
                 "'scale' is less than or equal to 0")
})


## 'Spline' -------------------------------------------------------------------

test_that("'Spline' works with valid inputs", {
    ans <- Spline()
    expect_s3_class(ans, "BayesRates_spec_spline")
    ans <- Spline(df = 3, scale = 0.33)
    expect_s3_class(ans, "BayesRates_spec_spline")
})

test_that("'Spline' raises correct error with invalid inputs", {
    expect_error(Spline(df = 1))
    expect_error(Spline(scale = 0),
                 "'scale' is less than or equal to 0")
})




## 'new' ----------------------------------------------------------------------

test_that("'new_*' functions work with valid inputs", {
    expect_s3_class(new_BayesRates_spec_timenull(),
                    c("BayesRates_spec_timenull", "BayesRates_spec"))
    expect_s3_class(new_BayesRates_spec_timefixed(scale = 0.5),
                    c("BayesRates_spec_timefixed", "BayesRates_spec"))
    expect_s3_class(new_BayesRates_spec_timevarying(scale = 0.1),
                    c("BayesRates_spec_timevarying", "BayesRates_spec"))
    expect_s3_class(new_BayesRates_spec_rw2(scale = 0.1),
                    c("BayesRates_spec_rw2", "BayesRates_spec"))
    expect_s3_class(new_BayesRates_spec_spline(df = 5L,
                                               scale = 0.1),
                    c("BayesRates_spec_spline", "BayesRates_spec"))
})
