
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293


## 'augment' ------------------------------------------------------------------

#' @importFrom generics augment
#' @export
generics::augment

## HAS_TESTS
#' Obtain estimates of rates, combined with original data
#'
#' Extract estimated rates from an object
#' created by a call to [smooth_age()]
#' or [smooth_agetime()], and combine these
#' with the original data.
#'
#' In addition to the modelled rates,
#' `augment()` also displayed 'direct'
#' estimates of rates. Direct estimates are
#' estimates obtained by dividing events
#' by populations at risk, with no smoothing.
#' 
#' @param x A `"BayesRates_results"` object created
#' by calling function [smooth_age()]
#' or [smooth_agetime()].
#' @param interval Width of credible intervals.
#' A number between 0 and 1. Default is `0.95`.
#' @param ... Not currently used.
#'
#' @returns A [tibble][tibble::tibble-package],
#' constructed by merging `nevent_df` and
#' `py_df`, and the creating six new columns:
#' - `<agevar>.mid`: Midpoints of age groups
#' (which is useful for plotting.)
#' - `.fitted`: Point estimates (posterior medians) of rates.
#' - `.lower`, `.upper`: Lower and upper bounds of
#' credible intervals specified by `interval`.
#' - `.probability`: A list column with all draws
#' from the posterior distribution.
#' - `.observed`: Direct estimates of rates.
#'
#' @seealso
#' - [smooth_age()] smooths rates over age.
#' - [smooth_agetime()] smooths rates over age and time.
#' - \code{\link[=components.BayesRates_results]{components()}}
#'   extracts rates, age effects, time effects, and hyper-parameters.
#' - \code{\link[=n_draw<-]{n_draw()}} sets the default number
#' of draws from the posterior distribution.
#' - [total_rate()] calculates a summary indicator.
#'
#' @examples
#' results <- smooth_agetime(nevent_df = nz_divorces,
#'                           py_df = nz_population,
#'                           age_width_df = nz_age_width_df,
#'                           age_min = 15)
#' augment(results)
#' @export
augment.BayesRates_results <- function(x,
                                       interval = 0.95,
                                       ...) {
    n_draw <- x$n_draw
    nevent_df <- x$nevent_df
    py_df <- x$py_df
    agevar <- x$agevar
    age_width_df <- x$age_width_df
    age_min <- x$age_min
    ans <- make_draws_post(x, n_draw = n_draw)
    ans <- ans[["rates"]]
    ans <- make_credible_intervals(ans, 
                                   measurevar = ".value",
                                   interval = interval)
    ans <- add_age_mid(ans,
                       agevar = agevar,
                       age_width_df = age_width_df,
                       age_min = age_min)
    f <- function(x, y) merge(x, y, sort = FALSE)
    ans <- Reduce(f, list(nevent_df, py_df, ans))
    ans$.observed <- with(ans, nevent / py)
    ans <- tibble::tibble(ans)
    ans
}


## 'components' ---------------------------------------------------------------

#' @importFrom generics components
#' @export
generics::components

## HAS_TESTS
#' Extract components from a results object
#'
#' Extract components from an object
#' created by a call to [smooth_age()]
#' or [smooth_agetime()].
#'
#' If the object was created by a call to
#' [smooth_age()], then the components that can be extracted are:
#' - `"rates"` Age-specific rates.
#' - `"intercept"` Model intercept.
#' - `"age_effect"` Age main effect.
#' - `"age_hyper"` Hyper-parameters for age main effect.
#'
#' If the object was created by a call to [smooth_agetime()],
#' then the components that can be extracted are
#' - `"rates"` Age-time-specific rates
#' - `"intercept"` Model intercept.
#' - `"age_effect"` Age main effect.
#' - `"time_effect"` Time main effect or age-time interaction.
#' - `"age_hyper"` Hyper-parametres for age main effect.
#' - `"time_hyper"` Hyper-parameters for time main effect or
#' age-time interaction.
#' 
#' @param object A results object.
#' @param what Components to be extracted.
#' If no value supplied, all available components
#' are extracted.
#' @param interval Width of credible intervals.
#' A number between 0 and 1. Default is `0.95`.
#' @param ... Not currently used.
#'
#' @returns A named list of tibbles, or, if `what`
#' is a single value, a single tibble.
#' Each tibble contains
#'
#' - classification variables, if present
#' - `.fitted` Point estimates (posterior medians).
#' - `.lower`, `.upper` Lower and upper bounds of
#' credible intervals.
#' - `.probability`. A list column with all draws
#' from the posterior distribution.
#' 
#' The age variable comes in versions: the original
#' version and one with a `.mid` suffix. The `.mid`
#' version is useful for plotting.
#'
#' @seealso
#' - [smooth_age()] smooths rates over age.
#' - [smooth_agetime()] smooths rates over age and time.
#' - \code{\link[=augment.BayesRates_results]{augment()}}
#' combines data and estimates for rates.
#' - \code{\link[=n_draw<-]{n_draw()}} sets the default number
#' of draws from the posterior distribution.
#' - [total_rate()] calculates a summary indicator
#'
#' @examples
#' ## run model
#' results <- smooth_age(nevent_df = nz_divorces_2020,
#'                       py_df = nz_population_2020,
#'                       age_width_df = nz_age_width_df,
#'                       age_min = 15)
#'
#' ## extract all components
#' components(results)
#'
#' ## extract age effect
#' components(results, what = "age_effect")
#' @export
components.BayesRates_results <- function(object,
                                          what = NULL,
                                          interval = 0.95,
                                          ...) {
    checkmate::assert_number(interval,
                             lower = 0,
                             upper = 1)
    n_draw <- object$n_draw
    agevar <- object$agevar
    age_width_df <- object$age_width_df
    age_min <- object$age_min
    draws_post <- make_draws_post(object = object,
                                  n_draw = n_draw)
    if (!is.null(what)) {
        nms <- names(draws_post)
        if (!all(what %in% nms)) {
            stop(gettextf("Invalid value for '%s' : valid choices are %s",
                          "what",
                          paste(sprintf("'%s'", nms), collapse = ", ")),
                 call. = FALSE)
        }
        draws_post <- draws_post[what]
    }
    ans <- lapply(draws_post,
                  make_credible_intervals,
                  measurevar = ".value",
                  interval = interval)
    has_age <- vapply(ans, function(x) agevar %in% names(x), TRUE)
    ans[has_age] <- lapply(ans[has_age],
                           add_age_mid,
                           agevar = agevar,
                           age_width_df = age_width_df,
                           age_min = age_min)
    if (length(ans) == 1L)
        ans <- ans[[1L]]
    ans
}


## 'n_draw' -------------------------------------------------------------------

#' Get and set the number of draws from the posterior distribution
#'
#' Query or specify the number of draws from the posterior
#' distribution that are generated by a results object.
#'
#' @param x An object of holding results, eg
#' an object generated by a call to
#' [smooth_age()] or [smooth_agetime()].
#' @param value Number of posterior draws. A non-negative
#' integer.
#'
#' @returns A modified version of `x`.
#'
#' @seealso
#' - [smooth_age()] smooths rates over age.
#' - [smooth_agetime()] smooths rates over age and time.
#' - \code{\link[=augment.BayesRates_results]{augment()}}
#' combines data and estimates for rates.
#' - \code{\link[=components.BayesRates_results]{components()}}
#' extracts rates, age effects,
#' time effects, and hyper-parameters.
#' - [total_rate()] calculates a summary indicator
#'
#' @examples
#' res <- smooth_age(nevent_df = nz_divorces_2020,
#'                   py_df = nz_population_2020,
#'                   age_width_df = nz_age_width_df,
#'                   age_min = 15)
#' n_draw(res)
#' n_draw(res) <- 20
#' n_draw(res)
#'
#' @export
n_draw <- function(x) {
  UseMethod("n_draw")
}

#' @export
n_draw.BayesRates_results <- function(x) {
  x$n_draw
}


#' @export
#' @rdname n_draw
`n_draw<-` <- function(x, value) {
  UseMethod("n_draw<-")
}

## HAS_TESTS
#' @export
#' @rdname n_draw
`n_draw<-.BayesRates_results` <- function(x, value) {
    value <- checkmate::assert_count(value,
                                     positive = TRUE,
                                     coerce = TRUE)
    x$n_draw <- value
    x
}


## 'print' --------------------------------------------------------------------

#' @export
print.BayesRates_results <- function(x, ...) {
    ## extract info
    spec_age <- x$spec_age
    spec_time <- x$spec_time
    agevar <- x$agevar
    timevar <- x$timevar
    byvar <- x$byvar
    n_draw <- x$n_draw
    has_time_var <- has_time_var(spec_time)
    has_byvar <- length(byvar) > 0L
    ## make strings
    str_title <- sprintf("--- Object of class \"%s\" ---\n",
                         class(x)[[1L]])
    str_lik <- sprintf("     nevent ~ Poisson(rate * py)\n")
    str_prior <- sprintf("  log(rate) = age_effect")
    if (has_time_var)
        str_prior <- paste0(str_prior, " + time_effect")
    str_prior <- paste0(str_prior, "\n")
    str_age <- sprintf(" age_effect ~ %s\n",
                       make_str(spec_age))
    if (has_time_var)
        str_time <- sprintf("time_effect ~ %s\n",
                            make_str(spec_time))
    str_agevar <- sprintf("   agevar: %s\n", agevar)
    str_timevar <- sprintf("  timevar: %s\n", timevar)
    val_byvar <- if (has_byvar) paste(byvar, collapse = ", ") else "<none>"
    str_byvar <- sprintf("    byvar: %s\n", val_byvar)
    str_n_draw <- sprintf("   n_draw: %s\n", n_draw)
    ## print
    cat(str_title)
    cat("\n")
    cat(str_lik)
    cat(str_prior)
    cat(str_age)
    if (has_time_var)
        cat(str_time)
    cat("\n")
    cat(str_agevar)
    if (has_time_var)
        cat(str_timevar)
    cat(str_byvar)
    cat(str_n_draw)
    ## return
    invisible(x)
}


## 'total_rate' --------------------------------------------------------------

#' Summarise rates by adding up over the life time
#'
#' Construct a summary measure for age-specific rates
#' by summing up the rates, weighted by the width of each
#' age group. The calculations are equivalent to those
#' used to construct the total fertility rate (TFR).
#'
#' @param x An object of class `"BayesRates_results"`,
#' constructed via a call to [smooth_age()] or
#' [smooth_agetime()].
#' @param interval Width of credible intervals.
#' A number between 0 and 1. Default is `0.95`.
#'
#' @returns A tibble with the following columns:
#' - the time variable specified by `timevar`,
#' if `x` was created by a call to [smooth_agetime()]
#' - further classifying variables, as specified by
#' `byvar` in the original call to [smooth_age()] or
#' [smooth_agetime()]
#' - `.fitted`. Point estimates from the model.
#' (Posterior medians.)
#' - `.lower`, `.upper`. Lower and upper limits
#' of the credible interval specified by `interval`.
#' - `.probability`. A list column containing all draws
#' from the posterior distribution.
#' - `.observed`. Total rates constructed from
#' direct estimates, ie events divided by population,
#' with no smoothing.
#'
#' @seealso
#' - [smooth_age()] smooths rates over age
#' - [smooth_agetime()] smooths rates over age and time
#' - \code{\link[=augment.BayesRates_results]{augment()}}
#' combines data and estimates for rates.
#' - \code{\link[=components.BayesRates_results]{components()}}
#' extracts rates, age effects,
#' time effects, and hyper-parameters.
#' - \code{\link[=n_draw<-]{n_draw()}} sets the default number
#' of draws from the posterior distribution.
#'
#' @examples
#' res <- smooth_agetime(nevent_df = cn_divorces,
#'                       py_df = cn_population,
#'                       spec_time = TimeFixed())
#' total_rate(res)
#' @export
total_rate <- function(x, interval = 0.95) {
  UseMethod("total_rate")
}

## HAS_TESTS
#' @export
total_rate.BayesRates_results <- function(x, interval = 0.95) {
    ## extract values
    agevar <- x$agevar
    timevar <- x$timevar
    byvar <- x$byvar
    nevent_df <- x$nevent_df
    py_df <- x$py_df
    age_width_df <- x$age_width_df
    agevar_val <- nevent_df$age
    n_draw <- x$n_draw
    ## check and tidy inputs
    n_draw <- checkmate::assert_int(n_draw,
                                    lower = 1L,
                                    coerce = TRUE)
    checkmate::assert_number(interval,
                             lower = 0,
                             upper = 1)
    ## make modelled estimates
    draws_post <- make_draws_post(object = x, n_draw = n_draw)
    rates <- draws_post[["rates"]]
    i_age <- match(rates[[agevar]], age_width_df[["age"]])
    width_age <- age_width_df$width[i_age]
    rate_width <- rates$.value * width_age
    by_ans <- intersect(names(rates), c("draw", timevar, byvar))
    ans <- stats::aggregate(rate_width, rates[by_ans], sum)
    names(ans)[[length(ans)]] <- ".value"
    ans <- make_credible_intervals(ans,
                                   measurevar = ".value",
                                   interval = interval)
    ## make direct estimates
    by_ag <- c(agevar, timevar, byvar)
    nevent_ag <- stats::aggregate(nevent_df["nevent"], nevent_df[by_ag], sum)
    py_ag <- stats::aggregate(py_df["py"], py_df[by_ag], sum)
    obs <- merge(nevent_ag, py_ag)
    i_age_obs <- match(obs[[agevar]], age_width_df[["age"]])
    width_age_obs <- age_width_df$width[i_age_obs]
    rate_width_obs <- (obs$nevent / obs$py) * width_age_obs
    by_obs <- setdiff(by_ans, "draw")
    obs <- stats::aggregate(rate_width_obs, obs[by_obs], sum)
    names(obs)[[length(obs)]] <- ".observed"
    ## merge and return
    ans <- merge(ans, obs, all.x = TRUE)
    ans <- tibble::tibble(ans)
    ans
}
