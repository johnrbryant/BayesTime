
#' Smooth rates over age
#'
#'
#' If the original data (arguments `nevent_df` and `py_df`
#' supplied to [smooth_age()] or [smooth_agetime()]) used
#' character or factor age labels that could not
#' be interpreted as integers, then `total_rate()`
#' needs help interpreting the labels. This help is
#' provided through the `age_width_df` argument.
#' `age_width_df` is a data frame showing the
#' the width to be used for each age group. If the
#' final age group is open (ie has no upper limit)
#' then the width should approximately equal the expected
#' number of years lived in that age group.
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
#' @param age_width_df A data frame with columns `"age"`
#' and `"width"`, giving the widths of each age group.
#' Required if the age group labels are not integers.
#' @param age_min The lower limit of the youngest age group.
#' Required if the age group labels are not integers.
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
                       age_width_df = NULL,
                       age_min = NULL,
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
    ## tidy age variables
    nevent_df[[agevar]] <- tidy_agevar(nevent_df[[agevar]])
    py_df[[agevar]] <- tidy_agevar(py_df[[agevar]])
    ## check age_width_df (using tidied age variable)
    has_age_width_df <- !is.null(age_width_df)
    agevar_val <- nevent_df[[agevar]]
    check_age_width_df_supplied(has_age_width_df = has_age_width_df,
                                agevar_val = agevar_val)
    if (has_age_width_df)
        check_age_width_df(age_width_df = age_width_df,
                           agevar_val = agevar_val)
    else
        age_width_df <- make_age_width_df(agevar_val)
    ## check age_min
    has_age_min <- !is.null(age_min)
    check_age_min_supplied(has_age_min = has_age_min,
                           agevar_val = agevar_val)
    if (!has_age_min)
        age_min <- min(agevar_val)
    ## construct datasets required for fitting
    nevent_ag <- stats::aggregate(nevent_df["nevent"],
                                  nevent_df[nms_classif_vars],
                                  sum)
    py_ag <- stats::aggregate(py_df["py"],
                              py_df[nms_classif_vars],
                              sum)
    df <- merge(nevent_ag, py_ag, by = nms_classif_vars)
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
                           age_width_df = age_width_df,
                           age_min = age_min,
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
#' @param age_width_df A data frame with columns `"age"`
#' and `"width"`, giving the widths of each age group.
#' Required if the age group labels are not integers.
#' @param age_min The lower limit of the youngest age group.
#' Required if the age group labels are not integers.
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
                           age_width_df = NULL,
                           age_min = NULL,
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
    ## tidy age and time variables
    nevent_df[[agevar]] <- tidy_agevar(nevent_df[[agevar]])
    py_df[[agevar]] <- tidy_agevar(py_df[[agevar]])
    nevent_df[[timevar]] <- tidy_timevar(nevent_df[[timevar]])
    py_df[[timevar]] <- tidy_timevar(py_df[[timevar]])
    ## check age_width_df (using tidied age variable)
    has_age_width_df <- !is.null(age_width_df)
    agevar_val <- nevent_df[[agevar]]
    check_age_width_df_supplied(has_age_width_df = has_age_width_df,
                                agevar_val = agevar_val)
    if (has_age_width_df)
        check_age_width_df(age_width_df = age_width_df,
                           agevar_val = agevar_val)
    else
        age_width_df <- make_age_width_df(agevar_val)
    ## check age_min
    has_age_min <- !is.null(age_min)
    check_age_min_supplied(has_age_min = has_age_min,
                           agevar_val = agevar_val)
    if (!has_age_min)
        age_min <- min(agevar_val)
    ## construct datasets required for fitting
    nevent_ag <- stats::aggregate(nevent_df["nevent"],
                                  nevent_df[nms_classif_vars],
                                  sum)
    py_ag <- stats::aggregate(py_df["py"],
                              py_df[nms_classif_vars],
                              sum)
    nevent_ag <- fill_times(nevent_ag, nms_classif_vars = nms_classif_vars)
    py_ag <- fill_times(py_ag, nms_classif_vars = nms_classif_vars)
    df <- merge(nevent_ag, py_ag, by = nms_classif_vars)
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
                           age_width_df = age_width_df,
                           age_min = age_min,
                           spec_age = spec_age,
                           spec_time = spec_time,
                           fitted = fitted,
                           vals_by = vals_by)
}
