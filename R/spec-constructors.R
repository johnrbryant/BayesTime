
## User-visible constructor ---------------------------------------------------

## HAS_TESTS
#' Specify time trends: age pattern fixed
#'
#' Specify time trends where the age pattern is
#' assumed to maintain the same shape over time,
#' and the time effect follows a random walk.
#'
#' @section Mathematical details:
#'
#' When the age pattern is fixed, the
#' model for log rates is
#'
#' \deqn{\eta_{at} = \alpha_a + \beta_t}
#'
#' where
#' - \eqn{\eta_{at}} denotes the rates (on a log scale)
#' for age group \eqn{a} during period \eqn{t}
#' - \eqn{\alpha_a} is an age effect
#' - \eqn{\beta_t} is a time effect
#'
#' and the time effect is modelled as
#' 
#' \deqn{\beta_t = \beta_{t-1} + \epsilon_t}
#'
#' where \eqn{\epsilon} is a normally-distributed error term,
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
#' of the error term. Defaults to 1.
#'
#' @returns An object of class `"BayesRates_spec_timefixed"`.
#'
#' @seealso
#' - [TimeVarying()] specifies time trends
#' where the age pattern varies
#' - [RW2()] creates a random walk specification for
#' age effects
#' - [Spline()] creates a spline specification for
#' age effects
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
#' When the age pattern varies, the
#' model for log rates is
#'
#' \deqn{\eta_{at} = \alpha_a + \beta_{at}}
#'
#' where
#' - \eqn{\eta_{at}} denotes the rates (on a log scale)
#' for age group \eqn{a} during period \eqn{t}
#' - \eqn{\alpha_a} is an age effect
#' - \eqn{\beta_{at}} is an age-time interaction
#'
#' and the age-time interaction is modelled as
#' 
#' \deqn{\beta_t = \beta_{t-1} + \epsilon_t}
#'
#' where
#' - \eqn{\beta_t} is a vector of age-specific terms
#' - \eqn{\epsilon_t} a vector drawn from a multivariate
#' normal with mean 0 and variance \eqn{V}
#'
#' Element \eqn{v_{ij}} of \eqn{V} has the form
#'
#' \deqn{v_{ij} = \rho^{|i-j|}\sigma^2}
#' 
#' Correlation coefficiant \eqn{\rho} has a Beta(2,2) prior.
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
#' of the error term. Defaults to 1.
#' 
#' @returns An object of class `"BayesRates_spec_timevarying"`.
#'
#' @seealso
#' - [TimeFixed()] specifies time trends where the
#' age pattern is fixed
#' - [RW2()] creates a random walk specification for
#' age effects
#' - [Spline()] creates a spline specification for
#' age effects
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
#' Specify a random walk model for an age effect
#'
#' Specify a second-order random walk model
#' for an age effect. In a second-order random
#' walk the change in increments, rather than the
#' increments themselves, are random. A second-order
#' random walk tends to be smoother than an
#' ordinary (first-order) random walk.
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
#' - [TimeFixed()] specifies time trends where the
#' age pattern is fixed
#' - [TimeVarying()] specifies time trends where the
#' age pattern varies
#' - [Spline()] creates a spline specification for
#' age effects
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
#' A P-spline model is flexible, but
#' favours profiles that are relatively smooth.
#'
#' @section Mathematical details:
#'
#' The model for the age effect, on the log scale, is
#'
#' \deqn{\beta = X \gamma}
#'
#' where
#' - \eqn{\beta} is a vector of age effects
#' - \eqn{X} is a matrix holding the basis functions
#' for the spline, with `df` columns
#' - \eqn{\gamma} is a vector of coefficients,
#' with `df` elements
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
#' Defaults to 7.
#' @param scale Scale for error term.
#' Defaults to 1.
#'
#' @returns An object of class `BayesRates_spec_spline`.
#'
#' @seealso
#' - [TimeFixed()] specifies time trends where the
#' age pattern is fixed
#' - [TimeVarying()] specifies time trends where the
#' age pattern varies
#' - [RW2()] creates a random walk specification for
#' age effects
#'
#' `Spline()` uses function [splines::bs()] to
#' generate matrix \eqn{X}.
#'
#' @examples
#' Spline()
#' Spline(df = 10)
#' @export
Spline <- function(df = 7, scale = 1) {
    df <- checkmate::assert_int(df,
                                lower = 2L,
                                coerce = TRUE)
    checkmate::assert_number(scale, lower = 0, finite = TRUE)
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
