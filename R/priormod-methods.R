
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293

## 'get_nm' ----------------------------------------------------------

#' Get the name for the prior that is used within TMB
#'
#' @param priormod An object of class "BayesTime_priormod"
#'
#' @returns A string
#'
#' @noRd
get_nm <- function(priormod) {
  UseMethod("get_nm")
}

#'@export
get_nm.BayesTime_priormod_ar1 <- function(priormod) {
  "ar1"
}

#'@export
get_nm.BayesTime_priormod_lineartrend <- function(priormod) {
  "lineartrend"
}

#'@export
get_nm.BayesTime_priormod_rw2 <- function(priormod) {
  "rw2"
}

#'@export
get_nm.BayesTime_priormod_spline <- function(priormod) {
  "spline"
}



