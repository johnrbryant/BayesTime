
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
#' @param model_age The prior model for the age dimension.
#' Current choices: `"Spline"`, `"RW2"` (second-order random walk).
#' @param model_time The prior model for the time dimension. Current choices:
#' `"AR1"`, `"LocalTrend"`.
#' @param n_draw Number of draws from posterior distribution
#' to use in output. Defaults to 1000.
#'
#' @returns An object of class `"BayesRates_smooth_agetime"`.
#'
#' @seealso [rates_age()], [rates_total()]
#'
#' @export
smooth.agetime <- function(nevent_df,
                           py_df,
                           agevar = "age",
                           timevar = "time",
                           byvar = character(),
                           model_age = RW2(),
                           model_time = Spline(),
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
    checkmate::assert_class(model_age,
                            classes = c("BayesRates_model_spline",
                                        "BayesRates_model_rw2"))
    checkmate::assert_class(model_time,
                            classes = c("BayesRates_model_ar1",
                                        "BayesRates_model_localtrend"))
    ## check 'n_draw'
    n_draw <- checkmate::assert_int(n_draw,
                                    lower = 1L,
                                    coerce = TRUE)
    ## construct datasets required for fitting
    nevent_df <- stats::aggregate(nevent["nevent"],
                                  nevent[nms_classif_vars],
                                  sum)
    py_df <- stats::aggregate(py["py"],
                              py[nms_classif_vars],
                              sum)
    df <- merge(nevent_df, py_df, by = nms_classif_vars)
    df[[agevar]] <- format_agevar(df[[agevar]])
    has_byvar <- length(byvar) > 0L
    if (has_byvar)
        data <- split(df, df[byvar])
    else
        data <- list(df)
    nevent <- .mapply(make_agetime_matrix,
                      dots = data,
                      MoreArgs = list(measurevar = "nevent",
                                      agevar = agevar,
                                      timevar = timevar))
    py <- .mapply(make_agetime_matrix,
                  dots = data,
                  MoreArgs = list(measurevar = "py",
                                  agevar = agevar,
                                  timevar = timevar))
    ## do fitting
    fitted <- .mapply(smooth_agetime_inner,
                      dots = list(nevent = nevent,
                                  py = py),
                      MoreArgs = list(model_age = model_age,
                                      model_time = model_time))
    ## generate draws from posterior distribution
    post_sample <- lapply(fitted, post_sample, n_draw = n_draw)
    ## REWRITE FROM HERE
    ## assemble data frame with results
    if (has_byvar) {
        ans <- lapply(data, function(x) x[1L, byvar, drop = FALSE])
        ans <- do.call(rbind, ans)
    }
    else
        ans <- data.frame()
    ans$smoothed <- smoothed
    ## return
    ans
}
