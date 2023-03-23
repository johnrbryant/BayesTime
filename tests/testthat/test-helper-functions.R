
## 'make_accum_matrix' --------------------------------------------------------

test_that("'make_accum_matrix' works", {
    set.seed(0)
    n <- 10
    m <- make_accum_matrix(n)
    x <- rnorm(n - 1)
    y <- as.numeric(m %*% x)
    expect_equal(diff(y), x)
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
