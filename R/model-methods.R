
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293

## 'get_consts' ---------------------------------------------------------------

#' Get constants for prior model
#'
#' @param model An object of class "BayesRates_model"
#'
#' @returns A vector of doubles
#'
#' @noRd
get_consts <- function(model) {
  UseMethod("get_consts")
}

## HAS_TESTS
#' @export
get_consts.BayesRates_model_ar1 <- function(model) {
    c(coef_min = model$coef_min,
      coef_max = model$coef_max,
      mean = model$mean,
      sd = model$sd,
      scale = model$scale)      
}

## HAS_TESTS
#' @export
get_consts.BayesRates_model_localtrend <- function(model) {
    c(scale_effect = model$scale_effect,
      scale_level = model$scale_level,
      scale_trend = model$scale_trend,
      coef_min = model$coef_min,
      coef_max = model$coef_max)
}

## HAS_TESTS
#' @export
get_consts.BayesRates_model_rw2 <- function(model) {
    c(scale = model$scale)
}

## HAS_TESTS
#' @export
get_consts.BayesRates_model_spline <- function(model) {
    c(df = model$df, scale = model$scale)
}


## 'get_par' ------------------------------------------------------------------

#' Get initial values for parameters
#'
#' Names in return values are ignored by TMB but are
#' useful for debugging in R.
#' 
#' @param model An object of class "BayesRates_model"
#' @param n_effect Number of elements of age or time effect
#'
#' @returns A vector of doubles
#'
#' @noRd
get_par <- function(model, n_effect) {
    UseMethod("get_par")
}

## HAS_TESTS
#' @export
get_par.BayesRates_model_ar1 <- function(model, n_effect) {
    logit <- function(x) log(x / (1 - x))
    coef_mid <- 0.5 * (model$coef_min + model$coef_max)
    par_hyper <- c(logit_phi = logit(coef_mid),
                   alpha = model$mean,
                   log_sd = 0)
    par_effect <- rep(0, times = n_effect)
    names(par_effect) <- paste0("effect_", seq_len(n_effect))
    c(par_hyper, par_effect)
}

## HAS_TESTS
#' @export
get_par.BayesRates_model_localtrend <- function(model, n_effect) {
    par_log_sd <- c(log_sd_effect = 0,
                    log_sd_level = 0,
                    log_sd_trend = 0)
    logit <- function(x) log(x / (1 - x))
    coef_mid <- 0.5 * (model$coef_min + model$coef_max)
    par_logit_phi <- c(logit_phi = logit(coef_mid))
    par_effect <- rep(0, times = n_effect)
    names(par_effect) <- paste0("effect_", seq_len(n_effect))
    par_level <- rep(0, times = n_effect)
    names(par_level) <- paste0("level_", seq_len(n_effect))
    par_trend <- rep(0, times = n_effect - 1L)
    names(par_trend) <- paste0("trend_", seq_len(n_effect - 1L))
    c(par_log_sd, par_logit_phi, par_effect, par_level, par_trend)
}

## HAS_TESTS
#' @export
get_par.BayesRates_model_rw2 <- function(model, n_effect) {
    par_log_cd <- c(log_sd = 0)
    par_coef <- rep(0, times = n_effect - 2L) ## loose 2 df due to RW prior
    names(par_coef) <- paste0("coef_", seq_len(n_effect - 2L))
    c(par_log_cd, par_coef)
}

## HAS_TESTS
#' @export
get_par.BayesRates_model_spline <- function(model, n_effect) {
    par_log_cd <- c(log_sd = 0)
    par_coef <- rep(0, times = n_effect - 2L) ## lose 2 df due to RW prior
    names(par_coef) <- paste0("coef_", seq_len(n_effect - 2L))
    c(par_log_cd, par_coef)
}


## 'get_X_age' ----------------------------------------------------------------

#' Construct matrix X to be used in prior model for age
#'
#' @param model An object of class "BayesRates_model"
#' @param n_age Number of age groups
#'
#' @returns A sparse matrix
#'
#' @noRd
get_X_age <- function(model, n_age) {
  UseMethod("get_X_age")
}

## HAS_TESTS
#' @export
get_X_age.BayesRates_model_rw2 <- function(model, n_age) {
    make_rw2_matrix(n_age)
}



get_X_age.BayesRates_model_spline <- function(model, n_age) {
    df <- model$df
    X1 <- make_rw2_matrix(n_age)
    X2 <- make_spline_matrix(n = n_age, df = df)
    X2 %*% X1
}


## 'print' --------------------------------------------------------------------

#' @export
print.BayesRates_model_ar1 <- function(x, ...) {
    nchar_offset <- 15
    cat("< Object of class \"", class(x)[[1L]], "\" >\n", sep = "")
    cat(sprintf("% *s: %s\n", nchar_offset, "coef_min", x$coef_min))
    cat(sprintf("% *s: %s\n", nchar_offset, "coef_max", x$coef_max))
    cat(sprintf("% *s: %s\n", nchar_offset, "mean", x$mean))
    cat(sprintf("% *s: %s\n", nchar_offset, "sd", x$sd))
    cat(sprintf("% *s: %s\n", nchar_offset, "scale", x$scale))
}

#' @export
print.BayesRates_model_localtrend <- function(x, ...) {
    nchar_offset <- 15
    cat("< Object of class \"", class(x)[[1L]], "\" >\n", sep = "")
    cat(sprintf("% *s: %s\n", nchar_offset, "scale_effect", x$scale_effect))
    cat(sprintf("% *s: %s\n", nchar_offset, "scale_level", x$scale_level))
    cat(sprintf("% *s: %s\n", nchar_offset, "scale_trend", x$scale_trend))
}

#' @export
print.BayesRates_model_rw2 <- function(x, ...) {
    nchar_offset <- 15
    cat("< Object of class \"", class(x)[[1L]], "\" >\n", sep = "")
    cat(sprintf("% *s: %s\n", nchar_offset, "scale", x$scale))
}

#' @export
print.BayesRates_model_spline <- function(x, ...) {
    nchar_offset <- 15
    cat("< Object of class \"", class(x)[[1L]], "\" >\n", sep = "")
    cat(sprintf("% *s: %s\n", nchar_offset, "df", x$df))
    cat(sprintf("% *s: %s\n", nchar_offset, "scale", x$scale))
}

    
