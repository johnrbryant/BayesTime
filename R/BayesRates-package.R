
#' Smooth demographic rates using Bayesian methods
#'
#' Use Bayesian methods to smooth demographic rates
#' over age and (optionally) over time. Calculations
#' are done internally using package TMB.
#'
#' The key functions are
#' - [smooth_age()] for smoothing over age only
#' - [smooth_agetime()] for simultaneously smoothing
#' over age and time
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom tibble tibble
#' @useDynLib BayesRates, .registration = TRUE
## usethis namespace: end
NULL
