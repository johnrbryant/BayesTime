
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293


## 'augment' ------------------------------------------------------------------

#' @importFrom generics augment
#' @export
generics::augment

## HAS_TESTS
#' Data and values from a results object
#'
#' @param x A `"BayesRates_results"` object created
#' by calling function [smooth_age()]
#' or [smooth_agetime()].
#' @param n_draw Number of draws from posterior
#' distribution. Default is `1000`.
#' @param width Width of credible intervals.
#' A number between 0 and 1. Default is `0.95`.
#' @param ... Not currently used.
#'
#' @returns A [tibble][tibble::tibble-package],
#' constructed by merging the original `nevent_df` and
#' `py_df` arguments to [smooth_agetime()],
#' plus four new columns:
#' - `.fitted` Point estimates (posterior medians) of rates.
#' - `.lower`, `.upper` Lower and upper bounds of
#' credible intervals specified by `width`
#' - `.observed` Direct estimates of rates.
#'
#' @examples
#' results <- smooth_agetime(nevent_df = nz_divorces,
#'                           py_df = nz_population)
#' augment(results)
#' @export
augment.BayesRates_results <- function(x,
                                       n_draw = 1000,
                                       width = 0.95,
                                       ...) {
    rates <- components(x,
                        what = "rates",
                        n_draw = n_draw,
                        width = width)
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
#' @param n_draw Number of draws from posterior distribution
#' to use in output. Default is `1000`.
#' @param width Width of credible intervals.
#' A number between 0 and 1. Default is `0.95`.
#' @param ... Not currently used.
#'
#' @returns The return value depends on the value supplied for `what`:
#' - If `what` is a single name (eg `"rates"`), then `components()`
#' returns a [tibbles][tibble::tibble-package]
#' - If `what` is two or more names (eg `c("rates", "age_effect")`),
#' then `components()` returns a named list of tibbles.
#'
#' @seealso [augment()]
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
                                          n_draw = 1000,
                                          width = 0.95,
                                          ...) {
    checkmate::assert_character(what,
                                any.missing = FALSE,
                                min.chars = 1L,
                                min.len = 1L,
                                unique = TRUE)
    n_draw <- checkmate::assert_int(n_draw,
                                    lower = 1L,
                                    coerce = TRUE)
    checkmate::assert_number(width,
                             lower = 0,
                             upper = 1)
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
                  width = width)
    if (length(what) == 1L)
        ans <- ans[[1L]]
    ans
}
    

## 'print' --------------------------------------------------------------------

#' @export
print.BayesRates_results <- function(x, ...) {
    cat("< Object of class \"", class(x)[[1L]], "\" >\n", sep = "")
    invisible(x)
}

