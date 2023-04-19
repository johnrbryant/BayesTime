
#' Smooth rates over age
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
#' @param byvar The names of classification variables in `nevent_df`
#' and `py_df`, other than age (eg `"sex"` or `"region"`).
#' Optional.
#' @param spec_age The prior model for the age effect.
#' Current choices: [Spline()] and [RW2()]. Defaults to
#' `Spline()`.
#'
#' @returns An object of class `"BayesRates_results"`.
#'
#' @export
smooth_age <- function(nevent_df,
                       py_df,
                       agevar = "age",
                       byvar = character(),
                       spec_age = Spline()) {
    ## check variable names
    checkmate::assert_string(agevar, min.chars = 1L)
    checkmate::assert_character(byvar,
                                min.chars = 1L,
                                any.missing = FALSE,
                                unique = TRUE)
    if (agevar %in% byvar)
        stop("'byvar' contains variable called \"", agevar, "\"")
    nms_classif_vars <- c(agevar, byvar)
    ## check 'nevent_df' and 'py_df'
    check_input_notime_df(df = nevent_df,
                          measurevar = "nevent",
                          agevar = agevar,
                          byvar = byvar)
    check_input_notime_df(df = py_df,
                          measurevar = "py",
                          agevar = agevar,
                          byvar = byvar)
    ## check 'spec_age'
    if (!inherits(spec_age,
                  c("BayesRates_spec_spline", "BayesRates_spec_rw2")))
        stop(gettextf("'%s' has class \"%s\"",
                      "spec_age",
                      class(spec_age)),
             call. = FALSE)
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
                     make_age_matrix,
                     measurevar = "nevent",
                     agevar = agevar)
    py <- lapply(data,
                 make_age_matrix,
                 measurevar = "py",
                 agevar = agevar)
    ## construct (null) spec_time
    spec_time <- new_BayesRates_spec_timenull()
    ## do fitting
    fitted <- .mapply(make_fitted,
                      dots = list(nevent = nevent,
                                  py = py),
                      MoreArgs = list(spec_age = spec_age,
                                      spec_time = spec_time))
    ## extract info on levels of 'by' variables
    vals_by <- make_vals_by(df = df, byvar = byvar)    
    ## make results object
    new_BayesRates_results(nevent_df = nevent_df,
                           py_df = py_df,
                           agevar = agevar,
                           timevar = NULL,
                           byvar = byvar,
                           spec_age = spec_age,
                           spec_time = spec_time,
                           fitted = fitted,
                           vals_by = vals_by)
}



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
#' Defaults to `TimeVarying()`.
#'
#' @returns An object of class `"BayesRates_results"`.
#'
#' @export
smooth_agetime <- function(nevent_df,
                           py_df,
                           agevar = "age",
                           timevar = "time",
                           byvar = character(),
                           spec_age = Spline(),
                           spec_time = TimeVarying()) {
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
    check_input_withtime_df(df = nevent_df,
                            measurevar = "nevent",
                            agevar = agevar,
                            timevar = timevar,
                            byvar = byvar)
    check_input_withtime_df(df = py_df,
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
    ## extract info on levels of 'by' variables
    vals_by <- make_vals_by(df = df, byvar = byvar)    
    ## make results object
    new_BayesRates_results(nevent_df = nevent_df,
                           py_df = py_df,
                           agevar = agevar,
                           timevar = timevar,
                           byvar = byvar,
                           spec_age = spec_age,
                           spec_time = spec_time,
                           fitted = fitted,
                           vals_by = vals_by)
}
