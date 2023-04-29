
## User-visible constructor ---------------------------------------------------

## HAS_TESTS
#' Specify time trends: age pattern fixed
#'
#' Specify time trends where the shape of
#' the age profile is assumed to be constant
#' over time, even though the level
#' may change. The level is assumed to follow
#' a random walk.
#'
#' @section Mathematical details:
#'
#' In the model underlying `TimeFixed()`,
#'
#' \deqn{\eta_{at} = \alpha_a + \beta_t}
#'
#' where
#' - \eqn{\eta_{at}} denotes (log) rates
#' for age group \eqn{a} during period \eqn{t},
#' - \eqn{\alpha_a} is an age effect,
#' - \eqn{\beta_t} is a time effect,
#'
#' and the time effect is modelled as
#' 
#' \deqn{\beta_t = \beta_{t-1} + \epsilon_t}
#'
#' where \eqn{\epsilon} is a normally-distributed error term
#' with mean \eqn{0} and standard deviation \eqn{\sigma}.
#'
#' Standard deviation \eqn{\sigma} has prior
#'
#' \deqn{\sigma \sim \text{N}^+(0, a^2)}
#'
#' where \eqn{\text{N}^+} denotes a half-normal distribution,
#' and a value for scale parameter \eqn{a}
#' is supplied by the user.
#'
#' @param scale Scale for the standard deviation
#' of the error term. Default is 1.
#'
#' @returns An object of class `"BayesRates_spec_timefixed"`.
#'
#' @seealso
#' - [TimeVarying()] is an alternative approach to modelling
#' time trends. `TimeVarying()` allows age profiles to
#' change over time.
#' - [RW2()] models age effects using a random walk.
#' - [Spline()] models age effects using splines.
#' 
#' @examples
#' TimeFixed()
#' TimeFixed(scale = 2)
#' @export
TimeFixed <- function(scale = 1) {
    checkmate::assert_number(scale, lower = 0, finite = TRUE)
    check_gt_zero(scale, nm = "scale")
    new_BayesRates_spec_timefixed(scale = as.double(scale))
}


## HAS_TESTS
#' Specify time trends: age pattern changes
#'
#' Specify time trends where the age pattern
#' is allowed to vary smoothly over time.
#'
#' @section Mathematical details:
#'
#' In the model underlying `TimeVarying()`,
#'
#' \deqn{\eta_{at} = \alpha_a + \beta_{at}}
#'
#' where
#' - \eqn{\eta_{at}} denotes (log) rates
#' for age group \eqn{a} during period \eqn{t},
#' - \eqn{\alpha_a} is an age effect,
#' - \eqn{\beta_{at}} is an age-time interaction,
#'
#' and the age-time interaction is modelled as
#' 
#' \deqn{\beta_t = \beta_{t-1} + \epsilon_t}
#'
#' where
#' - \eqn{\beta_t} is a vector of age-specific terms, and
#' - \eqn{\epsilon_t} is a vector drawn from a multivariate
#' normal with mean 0 and variance \eqn{V}
#'
#' Element \eqn{v_{ij}} of \eqn{V} has the form
#'
#' \deqn{v_{ij} = \rho^{|i-j|}\sigma^2}
#' 
#' Correlation coefficient \eqn{\rho} has a Beta(2,2) prior.
#'
#' Standard deviation \eqn{\sigma} has prior
#'
#' \deqn{\sigma \sim \text{N}^+(0, a^2)}
#'
#' where \eqn{\text{N}^+} denotes a half-normal distribution,
#' and a value for scale parameter \eqn{a}
#' is supplied by the user.
#'
#' @param scale Scale for the standard deviation
#' of the error term. Default is 1.
#' 
#' @returns An object of class `"BayesRates_spec_timevarying"`.
#'
#' @seealso
#' - [TimeFixed()] is an alternative approach to modelling
#' time trends. `TimeFixed()` assumes that age profiles are
#' constant over time.
#' - [RW2()] models age effects using a random walk.
#' - [Spline()] models age effects using splines.
#'
#' @examples
#' TimeVarying()
#' TimeVarying(scale = 0.1)
#' @export
TimeVarying <- function(scale = 1) {
    checkmate::assert_number(scale, lower = 0, finite = TRUE)
    check_gt_zero(scale, nm = "scale")
    new_BayesRates_spec_timevarying(scale = as.double(scale))
}


## HAS_TESTS
#' Specify a random walk model for the age effect
#'
#' Specify a second-order random walk model
#' for the age effect. A second-order
#' random walk tends to be smoother than an
#' ordinary (first-order) random walk,
#' while still being flexible enough to
#' capture distinctive features of the age profile
#' being modelled.
#'
#' @section Mathematical details:
#'
#' The model for the age effect, on the log scale, is
#'
#' \deqn{(\beta_a - \beta_{a-1}) - (\beta_{a-1} - \beta_{a-2}) \sim \text{N}(0, \sigma^2)}
#'
#' Parameter \eqn{\sigma} has prior
#'
#' \deqn{\sigma \sim \text{N}^+(0, a^2)}
#'
#' where \eqn{\text{N}^+} denotes a half-normal distribution,
#' and a value for \eqn{a} is supplied by the user.
#'
#' @param scale Scale for error term.
#' Defaults to 1.
#'
#' @returns An object of class `BayesRates_spec_rw2`.
#'
#' @seealso
#' - [Spline()] is an alternative approach to
#' modelling age effects, based on splines.
#' - [TimeFixed()] is used to model time trends where the
#' age pattern is fixed over time.
#' - [TimeVarying()] is used to model time trends where the
#' age pattern varies over time.
#'
#' @examples
#' RW2()
#' RW2(scale = 0.1)
#' @export
RW2 <- function(scale = 1) {
    checkmate::assert_number(scale, lower = 0, finite = TRUE)
    check_gt_zero(scale, nm = "scale")
    new_BayesRates_spec_rw2(scale = scale)
}


## HAS_TESTS
#' Specify a P-spline model for an age effect
#'
#' Specify a P-spline (penalised spline)
#' model for an age effect.
#' A P-spline is flexible, but
#' favours profiles that are relatively smooth.
#'
#' @section Mathematical details:
#'
#' The model for the age effect, on the log scale, is
#'
#' \deqn{\beta = X \gamma}
#'
#' where
#' - \eqn{\beta} is a vector of age effects,
#' - \eqn{X} is a matrix holding the basis functions
#' for the spline, with `df` columns, and
#' - \eqn{\gamma} is a vector of coefficients,
#' with `df` elements.
#'
#' The elements of \eqn{\gamma} are assumed to follow
#' a second order random walk,
#'
#' \deqn{(\gamma_a - \gamma_{a-1}) - (\gamma_{a-1} - \gamma_{a-2}) \sim \text{N}(0, \sigma^2)}
#'
#' Parameter \eqn{\sigma} has prior
#'
#' \deqn{\sigma \sim \text{N}^+(0, a^2)}
#'
#' where \eqn{\text{N}^+} denotes a half-normal distribution,
#' and a value for \eqn{a} is supplied by the user.
#'
#' @param df Degrees of freedom for spline.
#' The default is 0.7 times the number of age groups,
#' or 4, whichever is higher.
#' @param scale Scale for error term.
#' The default is 1.
#'
#' @returns An object of class `"BayesRates_spec_spline"`.
#'
#' @seealso
#' - [RW2()] is an alternative approach to
#' modelling age effects, based on a random walk.
#' - [TimeFixed()] specifies time trends where the
#' age pattern is fixed over time.
#' - [TimeVarying()] specifies time trends where the
#' age pattern varies over time
#'
#' `Spline()` uses function [splines::bs()] internally.
#'
#' @examples
#' Spline()
#' Spline(df = 10)
#' @export
Spline <- function(df = NULL, scale = 1) {
    checkmate::assert_int(df,
                          lower = 4L,
                          null.ok = TRUE,
                          coerce = TRUE)
    checkmate::assert_number(scale,
                             lower = 0,
                             finite = TRUE)
    check_gt_zero(scale, nm = "scale")
    new_BayesRates_spec_spline(df = df,
                                scale = scale)
}


## Internal constructors ------------------------------------------------------

## HAS_TESTS
#' New object of class "BayesRates_spec_null"
#'
#' Placeholder used in models with no time dimension.
#' No user-visible constructor, since this class
#' is never seen by users.
#'
#' @returns Object of class "BayesRates_spec_null"
#'
#' @noRd
new_BayesRates_spec_timenull <- function() {
    ans <- list()
    class(ans) <- c("BayesRates_spec_timenull", "BayesRates_spec")
    ans
}


## HAS_TESTS
#' New object of class "BayesRates_spec_timefixed"
#'
#' @param scale Double greater than 0
#'
#' @returns Object of class "BayesRates_spec_timefixed
#'
#' @noRd
new_BayesRates_spec_timefixed <- function(scale) {
    ans <- list(scale = scale)
    class(ans) <- c("BayesRates_spec_timefixed", "BayesRates_spec")
    ans
}


## HAS_TESTS
#' New object of class "BayesRates_spec_timevarying"
#'
#' @param scale Double greater than 0
#'
#' @returns Object of class "BayesRates_spec_timevarying
#'
#' @noRd
new_BayesRates_spec_timevarying <- function(scale) {
    ans <- list(scale = scale)
    class(ans) <- c("BayesRates_spec_timevarying", "BayesRates_spec")
    ans
}


## HAS_TESTS
#' New object of class "BayesRates_spec_rw2"
#'
#' @param scale Double greater than 0
#'
#' @returns Object of class "BayesRates_spec_rw2
#'
#' @noRd
new_BayesRates_spec_rw2 <- function(scale) {
    ans <- list(scale = scale)
    class(ans) <- c("BayesRates_spec_rw2", "BayesRates_spec")
    ans
}


## HAS_TESTS
#' New object of class "BayesRates_spec_spline"
#'
#' @param df Integer greater than or equal to 2
#' @param scale Double greater than 0
#'
#' @returns Object of class "BayesRates_spec_spline
#'
#' @noRd
new_BayesRates_spec_spline <- function(df, scale) {
    ans <- list(df = df,
                scale = scale)
    class(ans) <- c("BayesRates_spec_spline", "BayesRates_spec")
    ans
}
