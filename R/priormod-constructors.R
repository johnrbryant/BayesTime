

Spline <- function(df = 5L) {
    new_BayesTime_priormod_spline(df = df)
}

RW2 <- function() {
    new_BayesTime_priormod_rw2()
}

AR1 <- function(coef_min = 0.8,
                coef_max = 0.98,
                mean = 4,
                sd = 2,
                scale = 1) {
    new_BayesTime_priormod_ar1(coef_min = coef_min,
                               coef_max = coef_max,
                               mean =  mean,
                               sd = sd,
                               scale = scale)
}


LocalTrend <- function(scale_effect = 1,
                       scale_level = 1,
                       scale_trend = 1) {
    new_BayesTime_priormod_localtrend(scale_effect = scale_effect,
                                      scale_level = scale_level,
                                      scale_trend = scale_trend)
}


