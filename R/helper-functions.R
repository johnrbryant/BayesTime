

## HAS_TESTS
#' Matrix to add up elements of vector
#'
#' Matrix that returns c(0, cumsum(x))
#'
#' @param n Number of elements in answer.
#'
#' @returns Vector of length n
#'
#' @noRd
make_accum_matrix <- function(n) {
    s <- seq_len(n)
    i <- unlist(lapply(s[-n], function(i) s[(i + 1L):n]))
    j <- rep(s[-n], times = n - s[-n])
    x <- 1
    Matrix::sparseMatrix(i = i, j = j, x = x)
}


## HAS_TESTS
#' Matrix to center vector
#'
#' @param n Number of elements of vector.
#'
#' @returns Vector of length n
#'
#' @noRd
make_center_matrix <- function(n) {
    s <- seq_len(n)
    i <- rep(s, times = n)
    j <- rep(s, each = n)
    x <- ifelse(i == j, (n - 1) / n, -1 / n)
    Matrix::sparseMatrix(i = i, j = j, x = x)
}


## HAS_TESTS
#' Matrix to create second order random walk
#'
#' @param n Number of elements of random walk vector.
#'
#' @returns Vector of length n
#'
#' @noRd
make_rw2_matrix <- function(n) {
    m1 <- make_accum_matrix(n - 1L)   ## n-1 x n-2
    m2 <- make_center_matrix(n - 1L)  ## n-1 x n-1
    m3 <- make_accum_matrix(n)        ## n x n-1
    m4 <- make_center_matrix(n)       ## n x n
    m4 %*% m3 %*% m2 %*% m1           ## n x n-2
}


## HAS_TESTS
#' Make a matrix of spline basis functions
#'
#' @param n Number of elements vector being modelled
#' @param df Degrees of freedom
#'
#' @returns Matrix with n rows and df columns
#'
#' @noRd
make_spline_matrix <- function(n, df) {
    x <- seq_len(n)
    ans <- splines::bs(x = x, df = df)
    nr <- nrow(ans)
    nc <- ncol(ans)
    i <- rep(seq.int(from = 2L, to = nr), times = nc)
    j <- rep(seq_len(nc), each = nr - 1L)
    xx <- as.numeric(ans[-1, ])
    Matrix::sparseMatrix(i = i, j = j, x = xx)
}
