
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
#' @param model_age: The prior model for the age dimension.
#' Current choices: `"Spline"`, `"RW2"` (second-order random walk).
#' @param model_time The prior model for the time dimension. Current choices:
#' `"AR1"`, `"LocalTrend"`.
#'
#' @returns An object of class `"BayesRates_smooth_agetime"`.
#'
#' @seealso [rates_age()], [rates_total()]
#'
#' @export
smooth_agetime <- function(nevent_df,
                           py_df,
                           agevar = "age",
                           timevar = "time",
                           byvar = character(),
                           model_age = c("Spline", "RW2"),
                           model_time = c("AR1", "LocalTrend")) {
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
    check_same_classif(nevent_df = nevent_df,
                       py_df = py_df,
                       nms_classif_vars = nms_classif_vars)
    ## check 'model_age' and 'model_time'
    model_age <- match.arg(model_age)
    model_time <- match.arg(model_time)
    ## construct datasets required for fitting
    df <- merge(nevent_df[c(nms_classif_vars, "nevent")],
                py_df[c(nms_classif_vars, "py")],
                by = nms_classif_vars)
    df[[agevar]] <- format_agevar(df[[agevar]])
    df[[timevar]] <- format_timevar(df[[timevar]])
    has_byvar <- length(byvar) > 0L
    if (has_byvar)
        l <- split(df, df[byvar])
    else
        l <- list(df)
    make_nevent <- function(x)
        stats::xtabs(nevent ~ age + time, data = x, addNA = TRUE)
    make_py <- function(x)
        stats::xtabs(py ~ age + time, data = x, addNA = TRUE)
    nevent <- lapply(l, make_nevent)
    py <- lapply(l, make_py)
    ## do fitting
    smoothed <- .mapply(smooth_agetime_inner,
                        dots = list(nevent = nevent,
                                    py = py),
                        MoreArgs = list(model_age = model_age,
                                        model_time = model_time))
    ## assemble data frame with results
    if (has_byvar) {
        ans <- lapply(l, function(x) x[1L, byvar, drop = FALSE])
        ans <- do.call(rbind, ans)
    }
    else
        ans <- data.frame()
    ans$smoothed <- smoothed
    ## return
    ans
}
