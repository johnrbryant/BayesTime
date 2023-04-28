

#' Create object to hold results from functions
#' 'smooth_age' and 'smooth_agetime'
#'
#' The object holds multiple copies of some
#' objects, eg 'spec_age' is held in the
#' object itself, and in each element of
#' 'fitted'. This uses a tiny bit of extra space,
#' but is convenient.
#'
#' @param nevent_df Data frame describing events.
#' @param py_df Data frame describing exposure.
#' @param agevar Name of the age variable.
#' @param timevar Name of time variable, or NULL.
#' @param byvar Names of classification variables,
#' other than age and time.
#' @param age_width_df Data frame holding widths
#' of age groups.
#' @param age_min Lower limit of youngest age group
#' @param spec_age Specification of prior model for
#' age effect.
#' @param spec_time Specification of prior model for
#' time effect.
#' @param fitted List of tibbles with draws from
#' posterior distribution.
#' @param vals_by List of data frames with
#' levels of byvar.
#'
#' @returns An object of class "BayesRates_results_notime".
#'
#' @noRd
new_BayesRates_results <- function(nevent_df,
                                   py_df,
                                   agevar,
                                   timevar,
                                   byvar,
                                   age_width_df,
                                   age_min,
                                   spec_age,
                                   spec_time,
                                   fitted,
                                   vals_by) {
    n_draw <- 1000L
    ans <- list(nevent_df = nevent_df,
                py_df = py_df,
                agevar = agevar,
                timevar = timevar,
                byvar = byvar,
                age_width_df = age_width_df,
                age_min = age_min,
                spec_age = spec_age,
                spec_time = spec_time,
                fitted = fitted,
                vals_by = vals_by,
                n_draw = n_draw)
    class(ans) <- "BayesRates_results"
    ans
}
