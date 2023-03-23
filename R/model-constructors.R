
## User-visible constructor ---------------------------------------------------

## HAS_TESTS
#' Specify an AR1 model for a time effect
#'
#' Specify an AR1 (autoregression) model
#' for a time effect. The prior assumes that the
#' time effect will converge to a log-run average
#' specified by the user. The user can specify
#' whether this convergence is fast or slow.
#'
#' @section Mathematical details:
#'
#' The model for the time effect, on the log scale, is
#'
#' \deqn{\beta_t = \phi \beta_{t-1} + (1 - \phi) \alpha + \epsilon_t}
#'
#' where
#' - \eqn{\beta_t} is the time effect
#' - \eqn{\phi} is the autocorrelation coefficient
#' - \eqn{\alpha} is the long-run average
#' - \eqn{\epsilon} is a normally-distributed error term,
#' with mean \eqn{0} and standard deviation \eqn{\sigma}.
#'
#' Parameter \eqn{\phi} has prior
#'
#' \deqn{(\phi - m_0)/(m_1 - m_0) \sim \text{Beta}(2, 2)}
#'
#' with values for \eqn{m_0} and \eqn{m_1} being supplied
#' by the user.
#' 
#' Parameter \eqn{\alpha} has prior
#'
#' \deqn{\alpha \sim \text{N}(m, s^2)}
#'
#' with values for \eqn{m} and \eqn{s} being supplied
#' by the user.
#'
#' Standard deviation \eqn{\sigma} has prior
#'
#' \deqn{\sigma \sim \text{N}^+(0, a^2)}
#'
#' where \eqn{\text{N}^+} denotes a half-normal distribution,
#' and a value for \eqn{a} is supplied by the user.
#'
#' @param coef_min,coef_max Lower and upper limits
#' for the autocorrelation coefficient.
#' Defaults are 0.80 and 0.98.
#' @param mean,sd Mean and standard
#' deviation for the prior for
#' the long-run average (on the log scale).
#' Defaults are 4 and 2.
#' @param scale Scale for
#' standard deviation
#' of error term. Defaults to 1.
#'
#' @returns An object of class `BayesRates_model_ar1`.
#'
#' @seealso
#' - [LocalTrend()] creates a "local trend" model
#' for time effects
#' - [RW2()] creates a random walk model for
#' age effects
#' - [Spline()] creates a spline model for
#' age effects
#'
#' The default bounds for the autocorrelation coefficient
#' are based on the bounds in `forecast::est()`.
#'
#' @examples
#' AR1()
#' AR1(mean = 2)
#' @export
AR1 <- function(coef_min = 0.8,
                coef_max = 0.98,
                mean = 4,
                sd = 2,
                scale = 1) {
    checkmate::assert_number(coef_min, lower = 0, upper = 1)
    checkmate::assert_number(coef_max, lower = 0, upper = 1)
    if (coef_max <= coef_min)
        stop(gettextf("'%s' [%s] is less than or equal to '%s' [%s]",
                      "coef_max",
                      coef_max,
                      "coef_min",
                      coef_min),
             call. = FALSE)
    checkmate::assert_number(mean, finite = TRUE)
    checkmate::assert_number(sd, lower = 0, finite = TRUE)
    checkmate::assert_number(scale, lower = 0, finite = TRUE)
    check_gt_zero(scale, nm = "scale")
    new_BayesRates_model_ar1(coef_min = as.double(coef_min),
                             coef_max = as.double(coef_max),
                             mean =  as.double(mean),
                             sd = as.double(sd),
                             scale = as.double(scale))
}


## HAS_TESTS
#' Specify a local trend model for a time effect
#'
#' Specify a local trend model for a time effect.
#' The model assumes that the time effect follows
#' a random walk with drift, where the drift
#' itself can also change over time.
#'
#' @section Mathematical details:
#'
#' The model for the time effect, on the log scale, is
#'
#' \deqn{\beta_t = \alpha_{t-1} + \epsilon_t^{(\beta)}}
#' \deqn{\alpha_t = \alpha_{t-1} + \delta_{t-1} + \epsilon_t^{(\alpha)}}
#' \deqn{\delta_t = \delta_{t-1} + \epsilon_t^{(\delta)}}
#'
#' where
#' - \eqn{\beta_t} is the time effect
#' - \eqn{\alpha} is a 'level' term
#' - \eqn{\delta} is a 'trend' term
#' - \eqn{\epsilon^{(\beta)}} is normally-distributed error term
#' with mean 0 and standard deviation \eqn{\sigma^{(\beta)}}
#' - \eqn{\epsilon^{(\alpha)}} is normally-distributed error term
#' with mean 0 and standard deviation \eqn{\sigma^{(\alpha)}}
#' - \eqn{\epsilon^{(\delta)}} is normally-distributed error term
#' with mean 0 and standard deviation \eqn{\sigma^{(\delta)}}
#'
#' Parameter \eqn{\sigma^{(\beta)}} has prior
#'
#' \deqn{\sigma^{(\beta)} \sim \text{N}^+(0, a_{\beta}^2)}
#'
#' where \eqn{\text{N}^+} denotes a half-normal distribution,
#' and a value for \eqn{a_{\beta}} is supplied by the user.
#'
#' Parameter \eqn{\sigma^{(\alpha)}} has prior
#'
#' \deqn{\sigma^{(\alpha)} \sim \text{N}^+(0, a_{\alpha}^2)}
#'
#' and parameter \eqn{\sigma^{(\delta)}} has prior
#'
#' \deqn{\sigma^{(\delta)} \sim \text{N}^+(0, a_{\delta}^2)}
#'
#' @param scale_effect Scale
#' for error for time effect. Defaults to 1.
#' @param scale_level Scale
#' for error for level term. Defaults to 1.
#' @param scale_trend Scale for prior
#' for error for trend term. Defaults to 1.
#'
#' @returns An object of class `BayesRates_model_localtrend`.
#'
#' @seealso
#' - [AR1()] creates a autoregression model
#' for time effects
#' - [RW2()] creates a random walk model for
#' age effects
#' - [Spline()] creates a spline model for
#' age effects
#'
#' @examples
#' LocalTrend()
#' LocalTrend(scale_trend = 0.1)
#' @export
LocalTrend <- function(scale_effect = 1,
                       scale_level = 1,
                       scale_trend = 1) {
    checkmate::assert_number(scale_effect, lower = 0, finite = TRUE)
    checkmate::assert_number(scale_level, lower = 0, finite = TRUE)
    checkmate::assert_number(scale_trend, lower = 0, finite = TRUE)
    check_gt_zero(scale_effect, nm = "scale_effect")
    check_gt_zero(scale_level, nm = "scale_level")
    check_gt_zero(scale_trend, nm = "scale_trend")
    new_BayesRates_model_localtrend(scale_effect = scale_effect,
                                    scale_level = scale_level,
                                    scale_trend = scale_trend)
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
#' @returns An object of class `BayesRates_model_rw2`.
#'
#' @seealso
#' - [AR1()] creates a autoregression model
#' for time effects
#' - [LocalTrend()] creates a local trend model
#' for time effects
#' - [Spline()] creates a spline model for
#' age effects
#'
#' @examples
#' RW2()
#' RW2(scale = 0.1)
#' @export
RW2 <- function(scale = 1) {
    checkmate::assert_number(scale, lower = 0, finite = TRUE)
    check_gt_zero(scale, nm = "scale")
    new_BayesRates_model_rw2(scale = scale)
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
#' \deqn{\beta = X \alpha}
#'
#' where
#' - \eqn{\beta} is a vector of age effects
#' - \eqn{X} is a matrix holding the basis functions
#' for the spline, with `df` columns
#' - \eqn{\alpha} is a vector of coefficients,
#' with `df` elements
#'
#' The elements of \eqn{\alpha} are assumed to follow
#' a second order random walk,
#'
#' \deqn{(\alpha_a - \alpha_{a-1}) - (\alpha_{a-1} - \alpha_{a-2}) \sim \text{N}(0, \sigma^2)}
#'
#' Parameter \eqn{\sigma} has prior
#'
#' \deqn{\sigma \sim \text{N}^+(0, a^2)}
#'
#' where \eqn{\text{N}^+} denotes a half-normal distribution,
#' and a value for \eqn{a} is supplied by the user.
#'
#' @param df Degrees of freedom for spline.
#' Defaults to 5.
#' @param scale Scale for error term.
#' Defaults to 1.
#'
#' @returns An object of class `BayesRates_model_spline`.
#'
#' @seealso
#' - [AR1()] creates a autoregression model
#' for time effects
#' - [LocalTrend()] creates a local trend model
#' for time effects
#' - [RW2()] creates a random walk model for
#' age effects
#'
#' `Spline()` uses function [splines::bs()] to
#' generate matrix \eqn{X}.
#'
#' @examples
#' Spline()
#' Spline(df = 10)
#' @export
Spline <- function(df = 5L, scale = 1) {
    df <- checkmate::assert_int(df,
                                lower = 2L,
                                coerce = TRUE)
    checkmate::assert_number(scale, lower = 0, finite = TRUE)
    check_gt_zero(scale, nm = "scale")
    new_BayesRates_model_spline(df = df,
                                scale = scale)
}


## Internal constructors ------------------------------------------------------

## HAS_TESTS
#' New object of class "BayesRates_model_ar1"
#'
#' @param coef_min Double between 0 and 1
#' @param coef_max Double between 'coef_min' and 1
#' @param mean Double
#' @param sd Double greater than 0
#' @param scale Double greater than 0
#'
#' @returns Object of class "BayesRates_model_ar1
#'
#' @noRd
new_BayesRates_model_ar1 <- function(coef_min,
                                     coef_max,
                                     mean,
                                     sd,
                                     scale) {
    ans <- list(coef_min = coef_min,
                coef_max = coef_max,
                mean = mean,
                sd = sd,
                scale = scale)
    class(ans) <- c("BayesRates_model_ar1", "BayesRates_model")
    ans
}


## HAS_TESTS
#' New object of class "BayesRates_model_localtrend"
#'
#' @param scale_effect Double greater than 0
#' @param scale_level Double greater than 0
#' @param scale_trend Double greater than 0
#'
#' @returns Object of class "BayesRates_model_local_trend
#'
#' @noRd
new_BayesRates_model_localtrend <- function(scale_effect,
                                            scale_level,
                                            scale_trend) {
    ans <- list(scale_effect = scale_effect,
                scale_level = scale_level,
                scale_trend = scale_trend)
    class(ans) <- c("BayesRates_model_localtrend", "BayesRates_model")
    ans
}


## HAS_TESTS
#' New object of class "BayesRates_model_rw2"
#'
#' @param scale Double greater than 0
#'
#' @returns Object of class "BayesRates_model_rw2
#'
#' @noRd
new_BayesRates_model_rw2 <- function(scale) {
    ans <- list(scale = scale)
    class(ans) <- c("BayesRates_model_rw2", "BayesRates_model")
    ans
}


## HAS_TESTS
#' New object of class "BayesRates_model_spline"
#'
#' @param df Integer greater than or equal to 2
#' @param scale Double greater than 0
#'
#' @returns Object of class "BayesRates_model_spline
#'
#' @noRd
new_BayesRates_model_spline <- function(df, scale) {
    ans <- list(df = df,
                scale = scale)
    class(ans) <- c("BayesRates_model_spline", "BayesRates_model")
    ans
}
