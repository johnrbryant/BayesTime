
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293

## 'get_scale' ---------------------------------------------------------------

#' Get constants for prior spec
#'
#' @param spec An object of class "BayesRates_spec"
#'
#' @returns A vector of doubles
#'
#' @noRd
get_scale <- function(spec) {
  UseMethod("get_scale")
}

## HAS_TESTS
#' @export
get_scale.BayesRates_spec <- function(spec) {
    c(scale = spec$scale)      
}

## HAS_TESTS
#' @export
get_scale.BayesRates_spec_timenull <- function(spec) {
    c(scale = 0)
}


## 'get_transforms_hyper' -----------------------------------------------------

#' Get transformed needed to convert hyper-parameters back
#' to original units
#'
#' @param model An object of class "BayesRates_spec"
#'
#' @returns A named list of functions.
#'
#' @noRd
get_transforms_hyper <- function(spec) {
    UseMethod("get_transforms_hyper")
}

## HAS_TESTS
#' @export
get_transforms_hyper.BayesRates_spec_timefixed <- function(spec) {
    list(sd = exp)
}

## HAS_TESTS
#' @export
get_transforms_hyper.BayesRates_spec_timevarying <- function(model) {
    list(sd = exp,
         coef = function(x) exp(x) / (1 + exp(x)))
}

## HAS_TESTS
#' @export
get_transforms_hyper.BayesRates_spec_rw2 <- function(model) {
    list(sd = exp,
         slope = function(x) x)
}

## HAS_TESTS
#' @export
get_transforms_hyper.BayesRates_spec_spline <- function(model) {
    list(sd = exp,
         slope = function(x) x)
}


## 'has_time_var' -------------------------------------------------------------

#' Whether a prior model allows for a time variable
#'
#' @param spec An object of class "BayesRates_spec"
#'
#' @returns TRUE or FALSE
#'
#' @noRd
has_time_var <- function(spec) {
  UseMethod("has_time_var")
}

## HAS_TESTS
#' @export
has_time_var.BayesRates_spec_timenull <- function(spec) {
    FALSE
}

## HAS_TESTS
#' @export
has_time_var.BayesRates_spec_timefixed <- function(spec) {
    TRUE
}

## HAS_TESTS
#' @export
has_time_var.BayesRates_spec_timevarying <- function(spec) {
    TRUE
}



## 'is_interaction' ------------------------------------------------------------

#' Whether a prior model specifies an age-time interaction
#' (as opposed to a pure time effect)
#'
#' @param spec An object of class "BayesRates_spec"
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_interaction <- function(spec) {
  UseMethod("is_interaction")
}

## HAS_TESTS
#' @export
is_interaction.BayesRates_spec_timefixed <- function(spec) {
    FALSE
}

## HAS_TESTS
#' @export
is_interaction.BayesRates_spec_timevarying <- function(spec) {
    TRUE
}


## 'make_map' -----------------------------------------------------------------

#' Make 'map' argument for MakeADFun
#'
#' Make 'map' argument, which is used to turn off
#' parameter estimation for some parameters
#' 
#' @param spec An object of class "BayesRates_spec"
#'
#' @returns A list or NULL
#'
#' @noRd
make_map <- function(spec, labels_age) {
    UseMethod("make_map")
}


## HAS_TESTS
#' @export
make_map.BayesRates_spec_timenull <- function(spec) {
    list(log_sd_time = factor(NA),
         logit_rho_time = factor(NA),
         parfree_time = factor(NA))
}

## HAS_TESTS
#' @export
make_map.BayesRates_spec_timefixed <- function(spec) {
    list(logit_rho_time = factor(NA))
}

## HAS_TESTS
#' @export
make_map.BayesRates_spec_timevarying <- function(spec) {
    NULL
}


## 'make_parfree_age' ---------------------------------------------------------

#' Get initial values for free parameters for the age effect
#'
#' Names in return values are ignored by TMB but are
#' useful for debugging in R.
#' 
#' @param spec An object of class "BayesRates_spec"
#' @param labels_age Labels for age groups
#'
#' @returns A vector of doubles
#'
#' @noRd
make_parfree_age <- function(spec, labels_age) {
    UseMethod("make_parfree_age")
}

## HAS_TESTS
#' @export
make_parfree_age.BayesRates_spec_rw2 <- function(spec, labels_age) {
    n <- length(labels_age)
    ans <- rep(0, times = n - 2L)
    names(ans) <- paste0("parfree.", seq_len(n - 2L))
    ans
}

## HAS_TESTS
#' @export
make_parfree_age.BayesRates_spec_spline <- function(spec, labels_age) {
    n_age <- length(labels_age)
    df <- spec$df
    df <- make_df_spline(df = df, n = n_age)
    ans <- rep(0, times = df - 2L)
    names(ans) <- paste0("parfree.", seq_len(df - 2L))
    ans
}


## 'make_parfree_time' --------------------------------------------------------

#' Get initial values for free parameters for age-time effects
#'
#' Names in return values are ignored by TMB but are
#' useful for debugging in R.
#' 
#' @param spec An object of class "BayesRates_spec"
#' @param labels_age Labels for age groups
#' @param labels_time Labels for periods
#'
#' @returns A matrix
#'
#' @noRd
make_parfree_time <- function(spec, labels_age, labels_time) {
    UseMethod("make_parfree_time")
}


## HAS_TESTS
#' @export
make_parfree_time.BayesRates_spec_timenull <- function(spec,
                                                       labels_age,
                                                       labels_time) {
    matrix(0, nrow = 1L, ncol = 1L)
}

## HAS_TESTS
#' @export
make_parfree_time.BayesRates_spec_timefixed <- function(spec,
                                                        labels_age,
                                                        labels_time) {
    n_time <- length(labels_time)
    ans <- matrix(0, nrow = 1L, ncol = n_time - 1L)
    colnames(ans) <- paste0("parfree.", seq_len(n_time - 1L))
    ans
}

## HAS_TESTS
#' @export
make_parfree_time.BayesRates_spec_timevarying <- function(spec,
                                                          labels_age,
                                                          labels_time) {
    n_age <- length(labels_age)
    n_time <- length(labels_time)
    ans <- matrix(0, nrow = n_age, ncol = n_time - 1L)
    dimnames(ans) <- list(labels_age,
                          paste0("parfree_time.", seq_len(n_time - 1L)))
    ans
}

## 'make_random' --------------------------------------------------------------

#' Make 'random' argument for MakeADFun
#'
#' Make 'random' argument, which is used to identify
#' random effects
#' 
#' @param spec An object of class "BayesRates_spec"
#'
#' @returns A list or NULL
#'
#' @noRd
make_random <- function(spec) {
    UseMethod("make_random")
}

## HAS_TESTS
#' @export
make_random.BayesRates_spec <- function(spec) {
    c("parfree_age", "parfree_time")
}

## HAS_TESTS
#' @export
make_random.BayesRates_spec_timenull <- function(spec) {
    "parfree_age"
}

## HAS_TESTS
#' @export
make_random.BayesRates_spec_timenull <- function(spec) {
    "parfree_age"
}


## 'make_str' -----------------------------------------------------------------

#' Make string to use in printing results object
#'
#' @param spec An object of class "BayesRates_spec"
#'
#' @returns A string
#'
#' @noRd
make_str <- function(spec) {
    UseMethod("make_str")
}

## HAS_TESTS
#' @export
make_str.BayesRates_spec_timefixed <- function(spec) {
    scale_curr <- spec$scale
    scale_default <- TimeFixed()$scale
    is_default <- isTRUE(all.equal(scale_curr, scale_default))
    if (is_default)
        "TimeFixed()"
    else
        sprintf("TimeFixed(scale=%s)", scale_curr)
}

## HAS_TESTS
#' @export
make_str.BayesRates_spec_timevarying <- function(spec) {
    scale_curr <- spec$scale
    scale_default <- TimeVarying()$scale
    is_default <- isTRUE(all.equal(scale_curr, scale_default))
    if (is_default)
        "TimeVarying()"
    else
        sprintf("TimeVarying(scale=%s)", scale_curr)
}

## HAS_TESTS
#' @export
make_str.BayesRates_spec_rw2 <- function(spec) {
    scale_curr <- spec$scale
    scale_default <- RW2()$scale
    is_default <- isTRUE(all.equal(scale_curr, scale_default))
    if (is_default)
        "RW2()"
    else
        sprintf("RW2(scale=%s)", scale_curr)
}

## HAS_TESTS
#' @export
make_str.BayesRates_spec_spline <- function(spec) {
    scale_curr <- spec$scale
    df_curr <- spec$df
    scale_default <- Spline()$scale
    df_default <- Spline()$df
    is_default_scale <- isTRUE(all.equal(scale_curr, scale_default))
    is_default_df <- isTRUE(all.equal(df_curr, df_default))
    if (is_default_scale && is_default_df)
        "Spline()"
    else if (is_default_scale && !is_default_df)
        sprintf("Spline(df=%s)", df_curr)
    else if (!is_default_scale && is_default_df)
        sprintf("Spline(scale=%s)", scale_curr)
    else
        sprintf("Spline(scale=%s, df=%s)", scale_curr, df_curr)
}
                

## 'make_X_age_parfree' --------------------------------------------------------

#' Construct matrix X to contert free
#' parameters for age effect into constrained
#' parameters (ie random walk)
#'
#' @param spec An object of class "BayesRates_spec"
#' @param labels_age Labels for age groups
#'
#' @returns A sparse matrix
#'
#' @noRd
make_X_age_parfree <- function(spec, labels_age) {
  UseMethod("make_X_age_parfree")
}

## HAS_TESTS
#' @export
make_X_age_parfree.BayesRates_spec_rw2 <- function(spec, labels_age) {
    n_age <- length(labels_age)
    ans <- make_rw2_matrix(n_age)
    dimnames(ans) <- list(age = labels_age,
                          parfree = seq_len(ncol(ans)))
    ans
}


## HAS_TESTS
#' @export
make_X_age_parfree.BayesRates_spec_spline <- function(spec, labels_age) {
    n_age <- length(labels_age)
    df <- spec$df
    df <- make_df_spline(df = df, n = n_age)
    ans <- make_rw2_matrix(df)
    dimnames(ans) <- list(par = seq_len(nrow(ans)),
                          parfree = seq_len(ncol(ans)))
    ans
}


## 'make_X_age_subspace' --------------------------------------------------------

#' Construct matrix X to map from subspace for age
#' to age effect
#'
#' @param spec An object of class "BayesRates_spec"
#' @param labels_age Labels for age groups
#'
#' @returns A sparse matrix
#'
#' @noRd
make_X_age_subspace <- function(spec, labels_age) {
  UseMethod("make_X_age_subspace")
}

## HAS_TESTS
#' @export
make_X_age_subspace.BayesRates_spec_rw2 <- function(spec, labels_age) {
    n_age <- length(labels_age)
    ans <- Matrix::Diagonal(n_age)
    dimnames(ans) <- list(age = labels_age,
                          age = labels_age)
    ans
}


## HAS_TESTS
#' @export
make_X_age_subspace.BayesRates_spec_spline <- function(spec, labels_age) {
    n_age <- length(labels_age)
    df <- spec$df
    df <- make_df_spline(df = df, n = n_age)
    ans <- make_spline_matrix(n = n_age, df = df)
    dimnames(ans) <- list(age = labels_age,
                          par = seq_len(ncol(ans)))
    ans
}


## 'make_X_time' ---------------------------------------------------------------

#' Construct matrix X to be used in prior mode for age-time effect
#'
#' @param spec An object of class "BayesRates_spec"
#' @param labels_time Labels for periods
#'
#' @returns A sparse matrix
#'
#' @noRd
make_X_time <- function(spec, labels_time) {
  UseMethod("make_X_time")
}

## HAS_TESTS
#' @export
make_X_time.BayesRates_spec_timenull <- function(spec, labels_time) {
    Matrix::sparseMatrix(i = 1L, j = 1L, x = 0)
}

## HAS_TESTS
#' @export
make_X_time.BayesRates_spec_timefixed <- function(spec, labels_time) {
    n_time <- length(labels_time)
    ans <- make_rw_matrix(n_time)
    dimnames(ans) <- list(time = labels_time,
                          parfree = seq_len(ncol(ans)))
    ans
}

## HAS_TESTS
#' @export
make_X_time.BayesRates_spec_timevarying <- function(spec, labels_time) {
    n_time <- length(labels_time)
    ans <- make_rw_matrix(n_time)
    dimnames(ans) <- list(time = labels_time,
                          parfree = seq_len(ncol(ans)))
    ans
}


## 'n_hyper' ------------------------------------------------------------------

#' Number of hyper-parameters used by prior model
#'
#' @param spec An object of class "BayesRates_spec"
#'
#' @returns An integer
#'
#' @noRd
n_hyper <- function(spec) {
  UseMethod("n_hyper")
}

## HAS_TESTS
#' @export
n_hyper.BayesRates_spec_timenull <- function(spec) {
    0L
}

## HAS_TESTS
#' @export
n_hyper.BayesRates_spec_timefixed <- function(spec) {
    1L
}

## HAS_TESTS
#' @export
n_hyper.BayesRates_spec_timevarying <- function(spec) {
    2L
}

## HAS_TESTS
#' @export
n_hyper.BayesRates_spec_spline <- function(spec) {
    2L
}

## HAS_TESTS
#' @export
n_hyper.BayesRates_spec_rw2 <- function(spec) {
    2L
}


## 'print' --------------------------------------------------------------------

#' @export
print.BayesRates_spec_timefixed <- function(x, ...) {
    nchar_offset <- 15
    cat("< Object of class \"", class(x)[[1L]], "\" >\n", sep = "")
    cat(sprintf("% *s: %s\n", nchar_offset, "scale", x$scale))
}

#' @export
print.BayesRates_spec_timevarying <- function(x, ...) {
    nchar_offset <- 15
    cat("< Object of class \"", class(x)[[1L]], "\" >\n", sep = "")
    cat(sprintf("% *s: %s\n", nchar_offset, "scale", x$scale_effect))
}

#' @export
print.BayesRates_spec_rw2 <- function(x, ...) {
    nchar_offset <- 15
    cat("< Object of class \"", class(x)[[1L]], "\" >\n", sep = "")
    cat(sprintf("% *s: %s\n", nchar_offset, "scale", x$scale))
}

#' @export
print.BayesRates_spec_spline <- function(x, ...) {
    nchar_offset <- 15
    cat("< Object of class \"", class(x)[[1L]], "\" >\n", sep = "")
    cat(sprintf("% *s: %s\n", nchar_offset, "df", x$df))
    cat(sprintf("% *s: %s\n", nchar_offset, "scale", x$scale))
}

    
