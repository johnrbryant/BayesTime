
#' Create object to hold results from function
#' 'smooth.agetime'
#'
#' As the interface for BayesRates develops,
#' we may modify the "BayesRates_results"
#' class.
#'
#' @param nevent_df Data frame describing events.
#' @param py_df Data frame describing exposure.
#' @param agevar Name of the age variable.
#' @param timevar Name of the time variable.
#' @param byvar Names of classification variables,
#' other than age and time.
#' @param model_age Prior model for age effect.
#' Current choices: `"Spline"`, `"RW2"`.
#' @param model_time Prior model for time effect. Current choices:
#' `"AR1"`, `"LocalTrend"`.
#' @param post_draws List of tibbles with draws from
#' posterior distribution.
#'
#' @returns An object of class "BayesRates_results".
#'
#' @noRd
new_BayesRates_results <- function(nevent_df,
                                   py_df,
                                   agevar,
                                   timevar,
                                   byvar,
                                   model_age,
                                   model_time,
                                   post_draws) {
    ans <- list(nevent_df = nevent_df,
                py_df = py_df,
                agevar = agevar,
                timevar = timevar,
                byvar = byvar,
                model_age = model_age,
                model_time = model_time,
                post_draws = post_draws)
    class(ans) <- "BayesRates_results"
    ans
}
