
## Note that methods require an '@export' tag,
## even when the generic function is not exported
## https://github.com/r-lib/devtools/issues/2293


## 'augment' ------------------------------------------------------------------

#' @importFrom generics augment
#' @export
generics::augment

## HAS_TESTS
#' Data and values from a fitted model
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
#'                           model_age = RW2(),
#'                           model_time = AR1())
#' augment(results)
#' @export
augment.BayesRates_results <- function(x, ...) {
    nevent_df <- x$nevent_df
    py_df <- x$py_df
    rates <- x$post_draws$rates
    agevar <- x$agevar
    timevar <- x$timevar
    byvar <- x$byvar
    nms_classif_vars <- c(byvar, timevar, agevar)
    nms_ord <- c(nms_classif_vars, "draw")
    ord <- do.call(order, args = rates[nms_ord])
    rates <- rates[ord, ]
    n_draw <- max(rates$draw)
    m <- matrix(rates$value, nrow = n_draw)
    q <- matrixStats::colQuantiles(m, probs = c(0.025, 0.5, 0.975))
    rates <- unique(rates[nms_classif_vars])
    rates <- rates[rev(nms_classif_vars)]
    rates$.fitted <- q[, 2L]
    rates$.lower <- q[, 1L]
    rates$.upper <- q[, 3L]
    ans <- merge(nevent_df, py_df)
    ans <- merge(ans, rates)
    ans$.observed <- with(ans, nevent / py)
    ans
}


## 'print' --------------------------------------------------------------------

#' @export
print.BayesRates_results <- function(x, ...) {
    cat("< Object of class \"", class(x)[[1L]], "\" >\n", sep = "")
    invisible(x)
}

