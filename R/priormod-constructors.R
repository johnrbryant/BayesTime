
## User-visible constructor ---------------------------------------------------


AR1 <- function(coef_min = 0.8,
                coef_max = 0.98,
                mean = 4,
                sd = 2,
                scale = 1) {
    new_BayesRates_priormod_ar1(coef_min = coef_min,
                               coef_max = coef_max,
                               mean =  mean,
                               sd = sd,
                               scale = scale)
}

LocalTrend <- function(scale_effect = 1,
                       scale_level = 1,
                       scale_trend = 1) {
    new_BayesRates_priormod_localtrend(scale_effect = scale_effect,
                                      scale_level = scale_level,
                                      scale_trend = scale_trend)
}


RW2 <- function() {
    new_BayesRates_priormod_rw2()
}


Spline <- function(df = 5L) {
    new_BayesRates_priormod_spline(df = df)
}


## Internal constructors ------------------------------------------------------


new_BayesRates_priormod_ar1 <- function(coef_min,
                                       coef_max,
                                       mean,
                                       sd,
                                       scale) {
    ans <- list(coef_min = coef_min,
                coef_max = coef_max,
                mean = mean,
                sd = sd,
                scale = scale)
    class(ans) <- c("BayesRates_priormod_rw2", "BayesRates_priormod")
    ans
}


new_BayesRates_priormod_localtrend <- function(scale_effect,
                                              scale_level,
                                              scale_trend) {
    ans <- list(scale_effect = scale_effect,
                scale_level = scale_level,
                scale_trend = scale_trend)
    class(ans) <- c("BayesRates_priormod_localtrend", "BayesRates_priormod")
    ans
}


new_BayesRates_priormod_rw2 <- function() {
    ans <- list()
    class(ans) <- c("BayesRates_priormod_rw2", "BayesRates_priormod")
    ans
}

new_BayesRates_priormod_spline <- function(df) {
    ans <- list(df = df)
    class(ans) <- c("BayesRates_priormod_spline", "BayesRates_priormod")
    ans
}


