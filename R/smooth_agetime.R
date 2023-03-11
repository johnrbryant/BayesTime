
#' Smooth rates over age and time
#'
#' @param nevent_df A data frame with a column called `"nevent"`
#' containing the number of events, plus additional classification
#' variables.
#' @param py_df A data frame with a column called `"py"`
#' containing exposure, plus additional
#' classification variables.
#' @param timevar The name of the time variable in `nevent_df` and `py_df`.
#' Defaults to `"time"`.
#' @param agevar The name of the age variable in `nevent_df` and `py_df`.
#' Defaults to `"age"`.
#' @param byvar The names of classification variables in `nevent_df`
#' and `py_df`, other than age and time (eg `"sex"` or `"region"`).
#' Optional.
#' @param model_time The prior model for the time dimension. Current choices:
#' `"AR1"`, `"LocalTrend"`.
#' @param model_age: The prior model for the age dimension.
#' Current choices: `"Spline"`, `"RW2"` (second-order random walk).
#'
#' @returns An object of class `"BayesTime_smooth_agetime"`.
#'
#' @seealso [rates_age()], [rates_total()]
#'
#' @export
smooth_agetime <- function(nevent_df,
                           py_df,
                           timevar = "time",
                           agevar = "age",
                           byvar = character(),
                           model_time = c("AR1", "LocalTrend"),
                           model_age = c("Spline", "RW2")) {
  ## check inputs
  checkmate::assert_string(timevar, min.chars = 1L)
  checkmate::assert_string(agevar, min.chars = 1L)
  if (identical(timevar, agevar))
    stop("'timevar' and 'agevar' identical")
  checkmate::assert_character(byvar,
                              min.chars = 1L,
                              any.missing = FALSE,
                              unique = TRUE)
  check_input_df(df = nevent_df,
           measurevar = "nevent",
                  timevar = timevar,
                  agevar = agevar,
                  byvar = byvar)
  check_input_df(df = py_df,
           measurevar = "py",
                  timevar = timevar,
                  agevar = agevar,
                  byvar = byvar)

  NULL
}
