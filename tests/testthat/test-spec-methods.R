
## 'get_scale' ----------------------------------------------------------------

test_that("'get_scale' works with BayesRates_spec_timenull", {
    spec <- new_BayesRates_spec_timenull()
    expect_identical(get_scale(spec),
                     c(scale = 0))
})

test_that("'get_scale' works with BayesRates_spec_timefixed", {
    spec <- TimeFixed()
    expect_identical(get_scale(spec),
                     c(scale = 1))
})

test_that("'get_scale' works with BayesRates_spec_timevarying", {
    spec <- TimeVarying()
    expect_identical(get_scale(spec),
                     c(scale = 1))
})

test_that("'get_scale' works with BayesRates_spec_rw2", {
    spec <- RW2()
    expect_identical(get_scale(spec),
                     c(scale = 1))
})

test_that("'get_scale' works with BayesRates_spec_rw2", {
    spec <- Spline()
    expect_identical(get_scale(spec),
                     c(scale = 1))
})


## 'get_transforms_hyper' -----------------------------------------------------

test_that("'get_transforms_hyper' works with TimeFixed", {
    spec <- TimeFixed()
    ans <- get_transforms_hyper(spec)
    expect_identical(names(ans), "sd")
    expect_equal(ans[[1]](0.2), exp(0.2))
})

test_that("'get_transforms_hyper' works with TimeVarying", {
    spec <- TimeVarying()
    invlogit <- function(x) exp(x) / (1 + exp(x))
    ans <- get_transforms_hyper(spec)
    expect_identical(names(ans), c("sd", "coef"))
    expect_equal(ans[[1]](0.2), exp(0.2))
    expect_equal(ans[[2]](0.3), invlogit(0.3))
})

test_that("'get_transforms_hyper' works with spline", {
    spec <- Spline()
    ans <- get_transforms_hyper(spec)
    expect_identical(names(ans), c("sd", "slope"))
    expect_equal(ans[[1]](0.2), exp(0.2))
    expect_equal(ans[[2]](0.2), 0.2)
})

test_that("'get_transforms_hyper' works with rw2", {
    spec <- RW2()
    ans <- get_transforms_hyper(spec)
    expect_identical(names(ans), c("sd", "slope"))
    expect_equal(ans[[1]](0.2), exp(0.2))
    expect_equal(ans[[2]](0.2), 0.2)
})


## 'has_time_var' -----------------------------------------------------------

test_that("'has_time_var' works", {
    expect_false(has_time_var(new_BayesRates_spec_timenull()))
    expect_true(has_time_var(TimeFixed()))
    expect_true(has_time_var(TimeVarying()))
    expect_error(has_time_var(RW2()))
})


## 'is_interaction' -----------------------------------------------------------

test_that("'is_interaction' works", {
    expect_false(is_interaction(TimeFixed()))
    expect_true(is_interaction(TimeVarying()))
    expect_error(is_interaction(Spline()))
    expect_error(is_interaction(RW2()))
})


## 'make_map' -----------------------------------------------------------------

test_that("'make_map' works with spec_timenull", {
    spec <- new_BayesRates_spec_timenull()
    expect_identical(make_map(spec),
                     list(log_sd_time = factor(NA),
                          logit_rho_time = factor(NA),
                          parfree_time = factor(NA)))
})

test_that("'make_map' works with spec_timefixed", {
    spec <- TimeFixed()
    expect_identical(make_map(spec),
                     list(logit_rho_time = factor(NA)))
})

test_that("'make_map' works with spec_timevarying", {
    spec <- TimeVarying()
    expect_identical(make_map(spec),
                     NULL)
})


## 'make_parfree_age' ---------------------------------------------------------

test_that("'make_parfree_age' works with rw2", {
    spec <- RW2()
    labels_age <- c("0-4", "5-9", "10-14")
    expect_identical(make_parfree_age(spec, labels_age = labels_age),
                     c("parfree.1" = 0))
})

test_that("'make_parfree_age' works with spline", {
    spec <- Spline(df = 4L)
    labels_age <- c("0-4", "5-9", "10-14")
    expect_identical(make_parfree_age(spec, labels_age = labels_age),
                     c("parfree.1" = 0, "parfree.2" = 0))
})


## 'make_parfree_time' ---------------------------------------------------------

test_that("'make_parfree_time' works with timenull", {
    spec <- new_BayesRates_spec_timenull()
    labels_age <- c("0-4", "5-9", "10-14")
    labels_time <- 2001:2004
    expect_identical(make_parfree_time(spec,
                                       labels_age = labels_age,
                                       labels_time = labels_time),
                     matrix(0, nr = 1, nc = 1))
})

test_that("'make_parfree_time' works with timefixed", {
    spec <- TimeFixed()
    labels_age <- c("0-4", "5-9", "10-14")
    labels_time <- 2001:2004
    expect_identical(make_parfree_time(spec,
                                       labels_age = labels_age,
                                       labels_time = labels_time),
                     matrix(0, nr = 1, nc = 3,
                            dimnames = list(NULL,
                                            c("parfree.1", "parfree.2", "parfree.3"))))
})

test_that("'make_parfree_time' works with timevarying", {
    spec <- TimeVarying()
    labels_age <- c("0-4", "5-9", "10-14")
    labels_time <- 2001:2003
    expect_identical(make_parfree_time(spec,
                                       labels_age = labels_age,
                                       labels_time = labels_time),
                     matrix(0, nr = 3, nc = 2,
                            dimnames = list(labels_age,
                                            c("parfree_time.1", "parfree_time.2"))))
})


## 'make_str' -----------------------------------------------------------------

test_that("'make_str' works with TimeFixed", {
    spec <- TimeFixed()
    expect_identical(make_str(spec),
                     "TimeFixed()")
    spec <- TimeFixed(scale = 0.1)
    expect_identical(make_str(spec),
                     "TimeFixed(scale=0.1)")
})

test_that("'make_str' works with TimeVarying", {
    spec <- TimeVarying()
    expect_identical(make_str(spec),
                     "TimeVarying()")
    spec <- TimeVarying(scale = 0.1)
    expect_identical(make_str(spec),
                     "TimeVarying(scale=0.1)")
})

test_that("'make_str' works with RW2", {
    spec <- RW2()
    expect_identical(make_str(spec),
                     "RW2()")
    spec <- RW2(scale = 0.1)
    expect_identical(make_str(spec),
                     "RW2(scale=0.1)")
})

test_that("'make_str' works with Spline", {
    spec <- Spline()
    expect_identical(make_str(spec),
                     "Spline()")
    spec <- Spline(scale = 0.1)
    expect_identical(make_str(spec),
                     "Spline(scale=0.1)")
    spec <- Spline(df = 12)
    expect_identical(make_str(spec),
                     "Spline(df=12)")
    spec <- Spline(df = 12, scale = 3)
    expect_identical(make_str(spec),
                     "Spline(scale=3, df=12)")
})


## 'make_random' --------------------------------------------------------------

test_that("'make_random' works with timenull", {
    spec <- new_BayesRates_spec_timenull()
    expect_identical(make_random(spec),
                     "parfree_age")
})

test_that("'make_random' works with timefixed", {
    spec <- TimeFixed()
    expect_identical(make_random(spec),
                     c("parfree_age", "parfree_time"))
})

test_that("'make_random' works with timevarying", {
    spec <- TimeVarying()
    expect_identical(make_random(spec),
                     c("parfree_age", "parfree_time"))
})


## 'make_X_age_parfree' -------------------------------------------------------

test_that("'make_X_age_parfree' works with rw2", {
    labels_age <- c("0-4", "5-9", "10-14")
    spec <- RW2()
    ans_obtained <- make_X_age_parfree(spec, labels_age = labels_age)
    ans_expected <- make_rw2_matrix(n = 3)
    colnames(ans_expected) <- 1
    rownames(ans_expected) <- labels_age
    names(dimnames(ans_expected)) <- c("age", "parfree")
    expect_identical(ans_obtained, ans_expected)                     
})

test_that("'make_X_age_parfree' works with spline", {
    labels_age <- 0:10
    spec <- Spline(df = 6)
    ans_obtained <- make_X_age_parfree(spec, labels_age = labels_age)
    ans_expected <- make_rw2_matrix(n = 6)
    colnames(ans_expected) <- 1:4
    rownames(ans_expected) <- 1:6
    names(dimnames(ans_expected)) <- c("par", "parfree")
    expect_identical(ans_obtained, ans_expected)                     
})


## 'make_X_age_subspace' ------------------------------------------------------

test_that("'make_X_age_subspace' works with rw2", {
    labels_age <- c("0-4", "5-9", "10-14")
    spec <- RW2()
    ans_obtained <- make_X_age_subspace(spec, labels_age = labels_age)
    ans_expected <- Matrix::Diagonal(3)
    colnames(ans_expected) <- labels_age
    rownames(ans_expected) <- labels_age
    names(dimnames(ans_expected)) <- c("age", "age")
    expect_identical(ans_obtained, ans_expected)                     
})

test_that("'make_X_age_subspace' works with spline", {
    labels_age <- 0:10
    spec <- Spline(df = 6)
    ans_obtained <- make_X_age_subspace(spec, labels_age = labels_age)
    ans_expected <- make_spline_matrix(n = 11, df = 6)
    colnames(ans_expected) <- 1:6
    rownames(ans_expected) <- labels_age
    names(dimnames(ans_expected)) <- c("age", "par")
    expect_identical(ans_obtained, ans_expected)                     
})


## 'make_X_time' ---------------------------------------------------------------

test_that("'make_X_time' works with timefixed", {
    labels_time <- 2001:2003
    spec <- TimeFixed()
    ans_obtained <- make_X_time(spec, labels_time = labels_time)
    ans_expected <- make_rw_matrix(n = 3)
    colnames(ans_expected) <- 1:2
    rownames(ans_expected) <- labels_time
    names(dimnames(ans_expected)) <- c("time", "parfree")
    expect_identical(ans_obtained, ans_expected)                     
})


## 'n_hyper' ------------------------------------------------------------------

test_that("'n_hyper' works", {
    expect_identical(n_hyper(new_BayesRates_spec_timenull()), 0L)
    expect_identical(n_hyper(TimeFixed()), 1L)
    expect_identical(n_hyper(TimeVarying()), 2L)
    expect_identical(n_hyper(Spline()), 2L)
    expect_identical(n_hyper(RW2()), 2L)
})



