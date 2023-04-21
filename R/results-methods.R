
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293


## 'augment' ------------------------------------------------------------------

#' @importFrom generics augment
#' @export
generics::augment

## HAS_TESTS
#' Data and rates estimates from a results object
#'
#' Extract estimates rates from an object
#' created by a call to [smooth_age()]
#' or [smooth_agetime()], and combine these
#' with the original data.
#' 
#' @param x A `"BayesRates_results"` object created
#' by calling function [smooth_age()]
#' or [smooth_agetime()].
#' @param interval Width of credible intervals.
#' A number between 0 and 1. Default is `0.95`.
#' @param ... Not currently used.
#'
#' @returns A [tibble][tibble::tibble-package],
#' constructed by merging the original `nevent_df` and
#' `py_df` arguments to [smooth_agetime()],
#' plus four new columns:
#' - `.fitted` Point estimates (posterior medians) of rates.
#' - `.lower`, `.upper` Lower and upper bounds of
#' credible intervals specified by `interval`
#' - `.probability`. A list column with all draws
#' from the posterior distribution.
#' - `.observed` Direct estimates of rates.
#'
#' @seealso
#' - [components()] extracts rates, age effects,
#' time effects, and hyper-parameters
#' - [n_draw<-()] sets the default number
#' of draws from the posterior distribution
#'
#' @examples
#' results <- smooth_agetime(nevent_df = nz_divorces,
#'                           py_df = nz_population)
#' augment(results)
#' @export
augment.BayesRates_results <- function(x,
                                       interval = 0.95,
                                       ...) {
    rates <- components(x,
                        what = "rates",
                        interval = interval)
    nevent_df <- x$nevent_df
    py_df <- x$py_df
    ans <- Reduce(merge, list(nevent_df, py_df, rates))
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
#' - `"rates"` Rates disaggregated by variables specified
#' in `agevar` and `byvar` arguments.
#' - `"age_effect"` Age main effect.
#' - `"age_hyper"` Hyper-parameters for age main effect.
#'
#' If the object was created by a call to [smooth_agetime()],
#' then the components that can be extracted are
#' - `"rates"` Rates disaggregated by variables specified
#' in `agevar`, `timevar`, and `byvar` arguments.
#' - `"age_effect"` Age main effect.
#' - `"time_effect"` Time main effect or age-time interaction.
#' - `"age_hyper"` Hyper-parametres for age main effect.
#' - `"time_hyper"` Hyper-parameters for time main effect or
#' age-time interaction.
#' 
#' @param object A results object.
#' @param what Components to be extracted.
#' Default is `"rates"`.
#' @param interval Width of credible intervals.
#' A number between 0 and 1. Default is `0.95`.
#' @param ... Not currently used.
#'
#' @returns The return value depends on the value supplied for `what`:
#' - If `what` is a single name (eg `"rates"`), then `components()`
#' returns a [tibbles][tibble::tibble-package]
#' - If `what` is two or more names (eg `c("rates", "age_effect")`),
#' then `components()` returns a named list of tibbles.
#'
#' Each tibble contains classification variables,
#' plus the following columns:
#' - `.fitted` Point estimates (posterior medians).
#' - `.lower`, `.upper` Lower and upper bounds of
#' credible intervals.
#' - `.probability`. A list column with all draws
#' from the posterior distribution.
#'
#' @seealso
#' - [augment()] combines data and results for rates
#' - [n_draw<-()] sets the default number
#' of draws from the posterior distribution
#' 
#'
#' @examples
#' ## extract data for 2020 
#' nz_divorces_2020 <- subset(nz_divorces,
#'                            time == 2020,
#'                            select = c(age, sex, nevent))
#' nz_popn_2020 <- subset(nz_population,
#'                        time == 2020,
#'                        select = c(age, sex, py))
#'
#' ## run age-only model
#' results <- smooth_age(nevent_df = nz_divorces_2020,
#'                       py_df = nz_popn_2020)
#'
#' ## extract components
#' components(results, what = "age_effect")
#' components(results, what = c("rates", "age_effect"))
#' @export
components.BayesRates_results <- function(object,
                                          what = "rates",
                                          interval = 0.95,
                                          ...) {
    checkmate::assert_character(what,
                                any.missing = FALSE,
                                min.chars = 1L,
                                min.len = 1L,
                                unique = TRUE)
    checkmate::assert_number(interval,
                             lower = 0,
                             upper = 1)
    n_draw <- object$n_draw
    draws_post <- make_draws_post(object = object,
                                  n_draw = n_draw)
    nms <- names(draws_post)
    is_what_in_nms <- what %in% nms
    i_not_found <- match(FALSE, is_what_in_nms, nomatch = 0L)
    if (i_not_found > 0L) {
        stop(gettextf("\"%s\" is not a valid choice of component : valid choices are %s",
                      what[[i_not_found]],
                      paste(sprintf("\"%s\"", nms), collapse = ", ")),
             call. = FALSE)
    }
    ans <- draws_post[what]
    ans <- lapply(ans,
                  make_credible_intervals,
                  measurevar = ".value",
                  interval = interval)
    if (length(what) == 1L)
        ans <- ans[[1L]]
    ans
}


## 'n_draw' -------------------------------------------------------------------

#' Set the number of draws from the posterior distribution
#'
#' Specify the number of draws from the posterior
#' distribution that are generated by a results object from
#' [smooth_age()] or [smooth_agetime()].
#'
#' @param x An object of class `"BayesRates_results"`,
#' generated by a call to [smooth_age()] or [smooth_agetime()].
#' @param value Number of posterior draws. A non-negative
#' integer.
#'
#' @returns A modified version of `x`.
#'
#' @examples
#' res <- smooth_age(nevent_df = nz_divorces_2020,
#'                   py_df = nz_population_2020)
#' res
#' n_draw(res) <- 20
#' res
#'
#' @export
`n_draw<-` <- function(x, value) {
  UseMethod("n_draw<-")
}

## HAS_TESTS
#' @export
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
#' @param x An object of class `"BayesRates_results"`,
#' constructed via a call to [smooth_age()] or
#' [smooth_agetime()].
#' @param age_width_df A data frame with columns `"age"`
#' and `"width"`, giving the widths of each age group.
#' Required if the age group labels used in
#' [smooth_age()] and [smooth_agetime()] were
#' non-integer.
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
#' - [smooth_age()] and [smooth_agetime()] to estimate rates
#' - [components()] and [augment()] to extract rates
#'
#' @examples
#' ## example where age groups are integers
#' res_cn <- smooth_agetime(nevent_df = BayesRates::cn_divorces,
#'                          py_df = BayesRates::cn_population,
#'                          byvar = "sex",
#'                          spec_time = TimeFixed())
#' total_rate(res_cn)
#'
#' ## example where age groups are character
#' res_nz <- smooth_age(nevent_df = BayesRates::nz_divorces_2020,
#'                      py_df = BayesRates::nz_population_2020)
#' total_rate(res_nz, age_width_df = BayesRates::nz_age_width_df)
#' @export
total_rate <- function(x, age_width_df = NULL, interval = 0.95) {
  UseMethod("total_rate")
}

## HAS_TESTS
#' @export
total_rate.BayesRates_results <- function(x,
                                           age_width_df = NULL,
                                           interval = 0.95) {
    ## extract values
    agevar <- x$agevar
    timevar <- x$timevar
    byvar <- x$byvar
    nevent_df <- x$nevent_df
    py_df <- x$py_df
    agevar_val <- nevent_df$age
    n_draw <- x$n_draw
    ## check and tidy inputs
    has_age_width_df <- !is.null(age_width_df)
    has_char_age_labels <- is.character(agevar_val) || is.factor(agevar_val)
    if (has_char_age_labels && !has_age_width_df)
        stop(gettextf("'%s' uses non-integer age labels, but no '%s' argument supplied to function '%s'",
                      "nevent_df",
                      "age_width_df",
                      "total_rate"),
             call. = FALSE)
    if (has_age_width_df)
        check_age_width_df(age_width_df = age_width_df,
                           agevar_val = agevar_val)
    else
        age_width_df <- make_age_width_df(agevar_val)
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
    ans <- merge(ans, obs)
    ans <- tibble::tibble(ans)
    ans
}
