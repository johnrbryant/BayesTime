
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
    list(sd = exp)
}

## HAS_TESTS
#' @export
get_transforms_hyper.BayesRates_spec_spline <- function(model) {
    list(sd = exp)
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
    n <- length(labels_age)
    ans <- rep(0, times = n - 2L)
    names(ans) <- paste0("parfree.", seq_len(n - 2L))
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




## 'make_X_age' ---------------------------------------------------------------

#' Construct matrix X to be used in prior model for age
#'
#' @param spec An object of class "BayesRates_spec"
#' @param labels_age Labels for age groups
#'
#' @returns A sparse matrix
#'
#' @noRd
make_X_age <- function(spec, labels_age) {
  UseMethod("make_X_age")
}

## HAS_TESTS
#' @export
make_X_age.BayesRates_spec_rw2 <- function(spec, labels_age) {
    n_age <- length(labels_age)
    ans <- make_rw2_matrix(n_age)
    dimnames(ans) <- list(age = labels_age,
                          parfree = seq_len(ncol(ans)))
    ans
}


## HAS_TESTS
#' @export
make_X_age.BayesRates_spec_spline <- function(spec, labels_age) {
    n_age <- length(labels_age)
    df <- spec$df
    X1 <- make_rw2_matrix(df)
    X2 <- make_spline_matrix(n = n_age, df = df)
    ans <- X2 %*% X1
    dimnames(ans) <- list(age = labels_age,
                          parfree = seq_len(ncol(ans)))
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
    1L
}

## HAS_TESTS
#' @export
n_hyper.BayesRates_spec_rw2 <- function(spec) {
    1L
}


## 'print' --------------------------------------------------------------------

#' @export
print.BayesRates_spec_ar1 <- function(x, ...) {
    nchar_offset <- 15
    cat("< Object of class \"", class(x)[[1L]], "\" >\n", sep = "")
    cat(sprintf("% *s: %s\n", nchar_offset, "coef_min", x$coef_min))
    cat(sprintf("% *s: %s\n", nchar_offset, "coef_max", x$coef_max))
    cat(sprintf("% *s: %s\n", nchar_offset, "mean", x$mean))
    cat(sprintf("% *s: %s\n", nchar_offset, "sd", x$sd))
    cat(sprintf("% *s: %s\n", nchar_offset, "scale", x$scale))
}

#' @export
print.BayesRates_spec_localtrend <- function(x, ...) {
    nchar_offset <- 15
    cat("< Object of class \"", class(x)[[1L]], "\" >\n", sep = "")
    cat(sprintf("% *s: %s\n", nchar_offset, "scale_effect", x$scale_effect))
    cat(sprintf("% *s: %s\n", nchar_offset, "scale_level", x$scale_level))
    cat(sprintf("% *s: %s\n", nchar_offset, "scale_trend", x$scale_trend))
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

    
