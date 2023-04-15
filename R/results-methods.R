
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
#' by calling function [smooth.agetime()].
#' @param ... Not currently used.
#'
#' @returns A [tibble][tibble::tibble-package],
#' constructed by merging the original `nevent_df` and
#' `py_df` arguments to [smooth.agetime()],
#' plus four new columns:
#' - `.fitted` Point estimates (posterior medians) of rates.
#' - `.lower`, `.upper` Lower and upper bounds of
#' 95% credible intervals for `.fitted`.
#' - `.observed` Direct estimates of the rate.
#'
#' @examples
#' results <- smooth.agetime(nevent_df = nz_divorces,
#'                           py_df = nz_population,
#'                           spec_age = RW2(),
#'                           spec_time = AR1())
#' augment(results)
#' @export
augment.BayesRates_results <- function(x, ...) {
    width <- 0.95
    nevent_df <- x$nevent_df
    py_df <- x$py_df
    rates <- components(x, what = "rates", width = width)
    ans <- Reduce(merge, list(nevent_df, py_df, rates))
    ans$.observed <- with(ans, nevent / py)
    ans
}


## 'components' ---------------------------------------------------------------

#' @importFrom generics components
#' @export
generics::components

## NO_TESTS
#' Extract components from a results object
#'
#' Extract components from an object
#' of class `"BayesRates_results"`.
#'
#' The components that can be extracted are:
#' - `"rates"` Rates disaggregated by variables specified
#' in `agevar`, `timevar`, and `byvar` arguments.
#' - `"age_effect"` Age main effect.
#' - `"time_effect"` Time main effect.
#' - `"age_hyper"` Hyper-parametrs for age main effect.
#' - `"time_hyper"` Hyper-parametrs for time main effect.
#'
#' @param object A results object.
#' @param what Component to be extracted.
#' @param ... Not currently used.
#'
#' @returns A [tibble][tibble::tibble-package].
#'
#' @seealso [augment()]
#'
#' @examples
#' results <- smooth.agetime(nevent_df = nz_divorces,
#'                           py_df = nz_population,
#'                           spec_age = RW2(),
#'                           spec_time = AR1())
#' components(results, what = "age_effect")
#' components(results, what = "time_effect")
#' @export
components.BayesRates_results <- function(object, what, quantiles = TRUE, ...) {
    draws_post <- object$draws_post
    nms <- names(draws_post)
    if (!(what %in% nms)) {
        stop(gettextf("\"%s\" is not a valid choice of component : valid choices are %s",
                      what,
                      paste(sprintf("\"%s\"", nms), collapse = ", ")),
             call. = FALSE)
    }
    ans <- draws_post[[what]]
    if (quantiles)
        ans <- make_credible_intervals(ans,
                                       measurevar = ".value",
                                       width = 0.95)
    ans
}
    

## 'print' --------------------------------------------------------------------

#' @export
print.BayesRates_results <- function(x, ...) {
    cat("< Object of class \"", class(x)[[1L]], "\" >\n", sep = "")
    invisible(x)
}

