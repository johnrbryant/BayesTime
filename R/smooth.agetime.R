
#' Smooth rates over age and time
#'
#' @param nevent_df A data frame with a column called `"nevent"`
#' containing the number of events, plus additional classification
#' variables.
#' @param py_df A data frame with a column called `"py"`
#' containing exposure, plus additional
#' classification variables.
#' @param agevar The name of the age variable in `nevent_df` and `py_df`.
#' Defaults to `"age"`.
#' @param timevar The name of the time variable in `nevent_df` and `py_df`.
#' Defaults to `"time"`.
#' @param byvar The names of classification variables in `nevent_df`
#' and `py_df`, other than age and time (eg `"sex"` or `"region"`).
#' Optional.
#' @param model_age The prior model for the age effect.
#' Current choices: `"Spline"`, `"RW2"` (second-order random walk).
#' @param model_time The prior model for the time effect. Current choices:
#' `"AR1"`, `"LocalTrend"`.
#' @param n_draw Number of draws from posterior distribution
#' to use in output. Defaults to 1000.
#'
#' @returns An object of class `"BayesRates_smooth_agetime"`.
#'
#' @export
smooth.agetime <- function(nevent_df,
                           py_df,
                           agevar = "age",
                           timevar = "time",
                           byvar = character(),
                           model_age = RW2(),
                           model_time = AR1(),
                           n_draw = 1000L) {
    ## check variable names
    checkmate::assert_string(agevar, min.chars = 1L)
    checkmate::assert_string(timevar, min.chars = 1L)
    checkmate::assert_character(byvar,
                                min.chars = 1L,
                                any.missing = FALSE,
                                unique = TRUE)
    if (identical(agevar, timevar))
        stop("'agevar' and 'timevar' identical")
    for (vname in c(agevar, timevar))
        if (vname %in% byvar)
            stop("'byvar' contains variable called \"", vname, "\"")
    nms_classif_vars <- c(timevar, agevar, byvar)
    ## check 'nevent_df' and 'py_df'
    check_input_df(df = nevent_df,
                   measurevar = "nevent",
                   agevar = agevar,
                   timevar = timevar,
                   byvar = byvar)
    check_input_df(df = py_df,
                   measurevar = "py",
                   agevar = agevar,
                   timevar = timevar,
                   byvar = byvar)
    ## check 'model_age' and 'model_time'
    if (!inherits(model_age,
                  c("BayesRates_model_spline", "BayesRates_model_rw2")))
        stop(gettextf("'%s' has class \"%s\"",
                      "model_age",
                      class(model_age)),
             call. = FALSE)
    if (!inherits(model_time,
                  c("BayesRates_model_ar1", "BayesRates_model_localtrend")))
        stop(gettextf("'%s' has class \"%s\"",
                      "model_time",
                      class(model_time)),
             call. = FALSE)
    ## check 'n_draw'
    n_draw <- checkmate::assert_int(n_draw,
                                    lower = 1L,
                                    coerce = TRUE)
    ## construct datasets required for fitting
    nevent_ag <- stats::aggregate(nevent_df["nevent"],
                                  nevent_df[nms_classif_vars],
                                  sum)
    py_ag <- stats::aggregate(py_df["py"],
                              py_df[nms_classif_vars],
                              sum)
    df <- merge(nevent_ag, py_ag, by = nms_classif_vars)
    df[[agevar]] <- format_agevar(df[[agevar]])
    has_byvar <- length(byvar) > 0L
    if (has_byvar)
        data <- split(df, df[byvar])
    else
        data <- list(df)
    nevent <- lapply(data,
                     make_agetime_matrix,
                     measurevar = "nevent",
                     agevar = agevar,
                     timevar = timevar)
    py <- lapply(data,
                 make_agetime_matrix,
                 measurevar = "py",
                 agevar = agevar,
                 timevar = timevar)
    ## do fitting
    fitted <- .mapply(make_fitted,
                      dots = list(nevent = nevent,
                                  py = py),
                      MoreArgs = list(model_age = model_age,
                                      model_time = model_time))
    ## generate draws from posterior distribution
    post_draws <- lapply(fitted,
                         make_post_draws,
                         n_draw = n_draw,
                         nevent_df = nevent_df,
                         agevar = agevar,
                         timevar = timevar)
    ## merge with byvar and combine
    post_draws <- merge_byvar_with_post(post_draws = post_draws,
                                        data = data,
                                        byvar = byvar)
    ## make resultsobject
    ans <- new_BayesRates_results(nevent_df = nevent_df,
                                  py_df = py_df,
                                  agevar = agevar,
                                  timevar = timevar,
                                  byvar = byvar,
                                  model_age = model_age,
                                  model_time = model_time,
                                  post_draws = post_draws)
    ans
}
