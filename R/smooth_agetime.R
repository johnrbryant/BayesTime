
#' Smooth rates over age
#'
#' Use data on numbers of events and population
#' at risk to produce estimates of rates that
#' are smoothed over age. The amount of smoothing
#' depends on the model used to represent age effects.
#' Current choices are [Spline()] (the default) and
#' [RW2()]. See the documentation for `Spline()` and
#' `RW2()` for a description of the mathematical models
#' underlying the smoothing.
#'
#' Dataset `nevent_df` must contain a
#' variable called `nevent` giving counts of events,
#' and dataset `py_df` must contain a variable called
#' `py` giving the population at risk.
#'
#' `nevent_df` and `py_df` must also contain
#' an age variable. If the age variable users multi-year
#' age groups (eg `"15-19"` or `"65+"`), then
#' `smooth_age()` needs help interpreting these age groups.
#' A data frame `age_width_df` must be supplied giving
#' the width of each age group, as well as a
#' value `age_min` giving the lower limit of the
#' youngest age group. (Future versions of `BayesRates`
#' will try to parse age group labels automatically.)
#'
#' `nevent_df` and `py_df` must *not* contain a time variable.
#' To analyse data with age and time dimensions, use
#' function [smooth_agetime()].
#'
#' `nevent_df` and `py_df` can contain additional
#' classifying variables, such as sex and region.
#' Independent models are fitted for each combination
#' of the levels of these additional variables.
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
#' @seealso
#' - [smooth_agetime()] models rates that vary
#' over age and time.
#' - [Spline()] and [RW2()] specify age effects
#' - \code{\link[=augment.BayesRates_results]{augment()}}
#' combines data and estimates for rates.
#' - \code{\link[=components.BayesRates_results]{components()}}
#' extracts rates, age effects,
#' time effects, and hyper-parameters.
#' - \code{\link[=n_draw<-]{n_draw()}} sets the default number
#' of draws from the posterior distribution.
#' - [total_rate()] calculates a summary indicator.
#'
#' @examples
#' res <- smooth_age(nevent_df = nz_divorces_2020,
#'                   py_df = nz_population_2020,
#'                   age_width_df = nz_age_width_df,
#'                   age_min = 15)
#' res
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
    check_df_same_levels(nevent_df = nevent_ag, py_df = py_ag)
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
#' Use data on numbers of events and population
#' at risk to produce estimates of rates that
#' are smoothed over age and time. The amount of smoothing
#' depends on the model used to represent age
#' and time effects. Current choices for
#' age effects are [Spline()] (the default) and
#' [RW2()]. Current choices for time effects are
#' [TimeVarying()] (the default) and [TimeFixed()].
#' See the documentation for `Spline()`, `RW2()`,
#' `TimeVarying()`, and `TimeFixed()` for a
#' description of the mathematical models
#' underlying the smoothing.
#'
#' Dataset `nevent_df` must contain a
#' variable called `nevent` giving counts of events,
#' and dataset `py_df` must contain a variable called
#' `py` giving the population at risk.
#'
#' `nevent_df` and `py_df` must also contain
#' an age variable. If the age variable users multi-year
#' age groups (eg `"15-19"` or `"65+"`), then
#' `smooth_age()` needs help interpreting these age groups.
#' A data frame `age_width_df` must be supplied giving
#' the width of each age group, as well as a
#' value `age_min` giving the lower limit of the
#' youngest age group. (Future versions of `BayesRates`
#' will try to parse age group labels automatically.)
#' 
#' `nevent_df` and `py_df` must contain a time variable.
#' To analyse data without a time dimension, use
#' function [smooth_age()]. Times must be single
#' years, eg `2015`, `2016`, `2017`. There may be gaps,
#' eg `2010`, `2015`, `2020`: `smooth_agetime()`
#' automatically estimates values for the
#' missing periods.
#'
#' `nevent_df` and `py_df` can contain additional
#' classifying variables, such as sex and region.
#' Independent models are fitted for each combination
#' of the levels of these additional variables.
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
#' @seealso
#' - [smooth_age()] models rates that do not have
#' a time dimension.
#' - [Spline()] and [RW2()] specify age effects
#' - [TimeVarying()] and [TimeFixed()] specify time effects
#' - \code{\link[=augment.BayesRates_results]{augment()}}
#' combines data and estimates for rates.
#' - \code{\link[=components.BayesRates_results]{components()}}
#' extracts rates, age effects,
#' time effects, and hyper-parameters.
#' - \code{\link[=n_draw<-]{n_draw()}} sets the default number
#' of draws from the posterior distribution.
#' - [total_rate()] calculates a summary indicator.
#'
#' @examples
#' results <- smooth_agetime(nevent_df = nz_divorces,
#'                           py_df = nz_population,
#'                           age_width_df = nz_age_width_df,
#'                           age_min = 15)
#' results
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
