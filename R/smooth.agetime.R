
#' Smooth rates over age and time
#'
#' @param nevent_df A data frame with a column
#' called `"nevent"` containing the number of events,
#' plus additional classification variables.
#' @param py_df A data frame with a column called `"py"`
#' containing exposure, plus additional
#' classification variables.
#' @param agevar The name of the age variable in
#' `nevent_df` and `py_df`.
#' Defaults to `"age"`.
#' @param timevar The name of the time variable in
#' `nevent_df` and `py_df`.
#' Defaults to `"time"`.
#' @param byvar The names of classification variables in `nevent_df`
#' and `py_df`, other than age and time (eg `"sex"` or `"region"`).
#' Optional.
#' @param spec_age The prior model for the age effect.
#' Current choices: [Spline()] and [RW2()]. Defaults to
#' `Spline()`.
#' @param spec_time The prior model for the time effect.
#' Current choices: [TimeFixed()] and [TimeVarying()].
#' Defaults to `TimeFixed()`.
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
                           spec_age = Spline(),
                           spec_time = TimeFixed(),
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
    ## check 'spec_age' and 'spec_time'
    if (!inherits(spec_age,
                  c("BayesRates_spec_spline", "BayesRates_spec_rw2")))
        stop(gettextf("'%s' has class \"%s\"",
                      "spec_age",
                      class(spec_age)),
             call. = FALSE)
    if (!inherits(spec_time,
                  c("BayesRates_spec_timefixed", "BayesRates_spec_timevarying")))
        stop(gettextf("'%s' has class \"%s\"",
                      "spec_time",
                      class(spec_time)),
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
                      MoreArgs = list(spec_age = spec_age,
                                      spec_time = spec_time))
    ## generate draws from posterior distribution
    draws_post <- lapply(fitted,
                         make_draws_post,
                         n_draw = n_draw,
                         nevent_df = nevent_df,
                         agevar = agevar,
                         timevar = timevar)
    ## merge with byvar and combine
    draws_post <- merge_byvar_with_post(draws_post = draws_post,
                                        data = data,
                                        byvar = byvar)
    ## make resultsobject
    ans <- new_BayesRates_results(nevent_df = nevent_df,
                                  py_df = py_df,
                                  agevar = agevar,
                                  timevar = timevar,
                                  byvar = byvar,
                                  spec_age = spec_age,
                                  spec_time = spec_time,
                                  draws_post = draws_post)
    ans
}
