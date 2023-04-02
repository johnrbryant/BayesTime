
## 'format_agevar' ------------------------------------------------------------

test_that("'format_agevar' works with valid input", {
    expect_identical(format_agevar(1:3, "age"), 1:3)
    expect_identical(format_agevar(factor(c("a", "b")), "age"), factor(c("a", "b")))
    expect_identical(format_agevar(c("b", "a", "b"), "age"),
                     factor(c("b", "a", "b"), levels = c("b", "a")))
})
    
test_that("'format_agevar' raises correct error with invalid input", {
    expect_error(format_agevar(NULL, "age"),
                 "'age' has class \"NULL\"")
})


## 'make_accum_matrix' --------------------------------------------------------

test_that("'make_accum_matrix' works", {
    set.seed(0)
    n <- 10
    m <- make_accum_matrix(n)
    x <- rnorm(n - 1)
    y <- as.numeric(m %*% x)
    expect_equal(diff(y), x)
})


## 'make_agetime_matrix' ------------------------------------------------------

test_that("'make_agetime_matrix' works", {
    data <- expand.grid(time = 2000:2002, age = 0:1,
                        KEEP.OUT.ATTRS = FALSE)
    data$nevent <- 1
    ans_obtained <- make_agetime_matrix(data,
                                        measurevar = "nevent",
                                        agevar = "age",
                                        timevar = "time")
    ans_expected <- matrix(1, nrow = 2, ncol = 3,
                           dimnames = list(age = 0:1, time = 2000:2002))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_center_matrix' -------------------------------------------------------

test_that("'make_center_matrix' works", {
    set.seed(0)
    n <- 10
    m <- make_center_matrix(n)
    x <- rnorm(n)
    y <- as.numeric(m %*% x)
    expect_equal(sum(y), 0)
})


## 'make_rw2_matrix' ----------------------------------------------------------

test_that("'make_rw2_matrix' works", {
    set.seed(0)
    n <- 10
    m <- make_rw2_matrix(n)
    x <- rnorm(n - 2)
    y <- as.numeric(m %*% x)
    expect_equal(sum(y), 0)
    expect_equal(sum(diff(y)), 0)
    expect_equal(diff(diff(y)), x)
})


## 'make_spline_matrix' ----------------------------------------------------------

test_that("'make_spline_matrix' works", {
    set.seed(0)
    n <- 10
    df <- 5
    m <- make_spline_matrix(n = n, df = df)
    expect_equal(dim(m), c(10L, 5L))
    expect_equal(sum(as.matrix(m)[1,]), 0)
    expect_true(all(rowSums(as.matrix(m)[-1,]) > 0))
})
