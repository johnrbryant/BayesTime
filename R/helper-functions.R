
## HAS_TESTS
#' Combine posterior samples for intercept and age effect,
#' and then exponentiate,
#' to obtain a posterior sample for rates
#'
#' @param intercept Data frame containing posterior sample
#' for intercept
#' @param age_effect Data frame containing posterior sample
#' for age effect
#' @returns A tibble with columns 'draw', agevar, '.value'
#'
#' @noRd
combine_draws_effects_notime <- function(intercept,
                                           age_effect) {
    names(intercept)[match(".value", names(intercept))] <- ".intercept"
    names(age_effect)[match(".value", names(age_effect))] <- ".age_effect"
    ans <- merge(intercept, age_effect)
    ans$.value <- with(ans, exp(.intercept + .age_effect))
    ans <- ans[-match(c(".intercept", ".age_effect"), names(ans))]
    nms_ord <- setdiff(names(ans), ".value")
    ord <- do.call(order, ans[nms_ord])
    ans <- ans[ord, , drop = FALSE]
    ans <- tibble(ans)
    ans
}


## HAS_TESTS
#' Combine posterior samples for intercept, age effect,
#' and time effect, and then exponentiate,
#' to obtain a posterior sample for rates
#'
#' @param intercept Data frame containing posterior sample
#' for intercept
#' @param age_effect Data frame containing posterior sample
#' for age effect
#' @param time_effect Data frame containing posterior sample
#' for time effect or age-time interaction
#'
#' @returns A tibble with columns 'draw', agevar, timevar, '.value'
#'
#' @noRd
combine_draws_effects_withtime <- function(intercept,
                                  age_effect,
                                  time_effect) {
    names(intercept)[match(".value", names(intercept))] <- ".intercept"
    names(age_effect)[match(".value", names(age_effect))] <- ".age_effect"
    names(time_effect)[match(".value", names(time_effect))] <- ".time_effect"
    ans <- merge(intercept, age_effect)
    ans <- merge(ans, time_effect)
    ans$.value <- with(ans, exp(.intercept + .age_effect + .time_effect))
    ans <- ans[-match(c(".intercept", ".age_effect", ".time_effect"), names(ans))]
    nms_ord <- setdiff(names(ans), ".value")
    ord <- do.call(order, ans[nms_ord])
    ans <- ans[ord, , drop = FALSE]
    ans <- tibble(ans)
    ans
}



#' Add rows to datasets representing intermediate periods
#'
#' Assumes that dataset has all possible combinations of
#' existing levels of classification variables
#'
#' @param df A data frame
#' @param nms_classif_vars Names of classification variables
#'
#' @returns A tibble
#'
#' @noRd
fill_times <- function(df, nms_classif_vars) {
    time <- df$time
    time_filled <- seq.int(from = min(time), to = max(time))
    if (all(time_filled %in% time))
        return(df)
    levels_classif_vars_filled <- lapply(df[nms_classif_vars], unique)
    levels_classif_vars_filled$time <- time_filled
    classif_vars_filled <- expand.grid(levels_classif_vars_filled,
                                       KEEP.OUT.ATTRS = FALSE,
                                       stringsAsFactors = FALSE)
    ans <- merge(x = df,
                 y = classif_vars_filled,
                 by = nms_classif_vars,
                 all.y = TRUE)
    ans <- tibble::tibble(ans)
    ord <- do.call(order, ans[nms_classif_vars])
    ans <- ans[ord, , drop = FALSE]
    ans
}


## HAS_TESTS
#' Test whether a character vector can be
#' coerced to integer without creating
#' new NAs
#'
#' @param A character vector
#'
#' @returns TRUE or FALSE
#'
#' @noRd
is_char_int <- function(x) {
    if (is.factor(x))
        x <- levels(x)
    x_obs <- x[!is.na(x)]
    x_obs_int <- suppressWarnings(as.integer(x_obs))
    !anyNA(x_obs_int) && all(x_obs_int == x_obs)
}


## HAS_TESTS
#' Matrix to add up elements of vector
#'
#' Matrix that returns c(0, cumsum(x))
#'
#' @param n Number of elements in answer.
#'
#' @returns Vector of length n
#'
#' @noRd
make_accum_matrix <- function(n) {
    s <- seq_len(n)
    i <- unlist(lapply(s[-n], function(i) s[(i + 1L):n]))
    j <- rep(s[-n], times = n - s[-n])
    x <- 1
    Matrix::sparseMatrix(i = i, j = j, x = x)
}


## HAS_TESTS
#' Make column matrix holding event or person-year data by age
#'
#' @param data Data frame with age, time, and measurement variables
#' @param measurevar Name of measurement variable
#' @param agevar Name of age variable
#'
#' @returns A matrix with a single column
#'
#' @noRd
make_age_matrix <- function(data, measurevar, agevar) {
    formula <- sprintf("%s ~ %s", measurevar, agevar)
    formula <- stats::as.formula(formula)
    ans <- stats::xtabs(formula, data = data, addNA = TRUE)
    matrix(ans,
           nrow = length(ans),
           ncol = 1L,
           dimnames = c(dimnames(ans), list(NULL)))
}



#' Make an 'age_width_df' matrix (as required by function
#' 'total_rate') from an 'agevar_val' vector consisting
#' of integer ages
#'
#' @param agevar_val
#'
#' @returns A data frame with two columns
#'
#' @noRd
make_age_width_df <- function(agevar_val) {
    age <- unique(agevar_val)
    age <- sort(age)
    n_age <- length(age)
    width <- rep(1, times = n_age)
    data.frame(age = age, width = width)
}


## HAS_TESTS
#' Make matrix holding event or person-year data by age by time
#'
#' @param data Data frame with age, time, and measurement variables
#' @param measurevar Name of measurement variable
#' @param agevar Name of age variable
#' @param timevar Name of time variable
#'
#' @returns A matrix
#'
#' @noRd
make_agetime_matrix <- function(data, measurevar, agevar, timevar) {
    formula <- sprintf("%s ~ %s + %s", measurevar, agevar, timevar)
    formula <- stats::as.formula(formula)
    ans <- stats::xtabs(formula, data = data, addNA = TRUE)
    array(ans,
          dim = dim(ans),
          dimnames = dimnames(ans))
}


## HAS_TESTS
#' Matrix to center vector
#'
#' @param n Number of elements of vector.
#'
#' @returns Vector of length n
#'
#' @noRd
make_center_matrix <- function(n) {
    s <- seq_len(n)
    i <- rep(s, times = n)
    j <- rep(s, each = n)
    x <- ifelse(i == j, (n - 1) / n, -1 / n)
    Matrix::sparseMatrix(i = i, j = j, x = x)
}


## HAS_TESTS
#' Create credible intervals from draws for a measure variable
#'
#' Modify a data frame so it replaces "draw" and measurevar
#' columns with columns ".fitted", ".lower", ".upper",
#' and .value,
#' (and shrink the number of rows by a factor equal
#' to the number of draws).
#'
#' Uses matrixStats::colQuantiles for speed.
#'
#' @param x A data frame
#' @param measurevar Name of the measurement variable
#' @param interval With of credible intervals
#'
#' @returns A data frame
#'
#' @noRd
make_credible_intervals <- function(x, measurevar, interval) {
    probs <- make_probs(interval)
    nms <- names(x)
    nms_classif <- setdiff(nms, c(measurevar, "draw"))
    n_classif <- length(nms_classif)
    if (n_classif > 0L) {
        nms_ord <- c(nms_classif, "draw")
        x_ord <- x[nms_ord]
        if (any(duplicated(x_ord)))
            stop(gettextf("duplicate combinations of variables %s",
                          paste(sprintf("'%s'", nms_ord), collapse = ", ")),
                 call. = FALSE)
        ord <- do.call(order, args = x_ord)
        x <- x[ord, , drop = FALSE]
        n_draw <- max(x$draw)
        m <- matrix(x[[measurevar]], nrow = n_draw)
        q <- matrixStats::colQuantiles(m, probs = probs)
        ans <- unique(x[nms_classif])
        ans$.fitted <- q[, 2L]
        ans$.lower <- q[, 1L]
        ans$.upper <- q[, 3L]
        ans$.probability <- apply(m, 2L, function(x) x, simplify = FALSE)
        ans <- tibble::tibble(ans)
    }
    else {
        q <- stats::quantile(x[[measurevar]], probs = probs)
        ans <- tibble::tibble(.fitted = q[[2L]],
                              .lower = q[[1L]],
                              .upper = q[[3L]],
                              .probability = list(x[[measurevar]]))
    }
    ans
}


## HAS_TESTS
#' Matrix to diff a vector
#'
#' @param n Number of elements of vector.
#'
#' @returns Sparse matrix with dim c(n-1, n)
#'
#' @noRd
make_diff_matrix <- function(n) {
    m <- matrix(0L, nrow = n - 1L, ncol = n)
    row <- row(m)
    col <- col(m)
    m[row == col] <- -1L
    m[row == col - 1L] <- 1L
    is_non_zero <- m != 0L
    i <- row[is_non_zero]
    j <- col[is_non_zero]
    x <- m[is_non_zero]
    Matrix::sparseMatrix(i = i, j = j, x = x)
}


## HAS_TESTS
#' Construct draws of age effect from draws
#' of joint posterior of all unknowns
#'
#' Assumes that 'slope' parameters is at position
#' offset - 1L
#'
#' @param draws_all A matrix with draws from the joint
#' distribution. Each row is one draw.
#' @param offset Index of first column of age effect
#' @param X_age_parfree Matrix mapping free parameters to
#' constrained parameters.
#' @param X_age_subspace Matrix mapping age subspace to
#' age effect
#' @param agevar Name of age variable
#' @param age_hyper Data frame with hyper-parameters
#' for prior model for age
#'
#' @returns A tibble with columns 'draw', agevar, 'age_effect'
#'
#' @noRd
make_draws_age_effect <- function(draws_all, offset, X_age_parfree, X_age_subspace, agevar, age_hyper) {
    ## extract parameters
    n_draw <- nrow(draws_all)
    X_age_parfree <- as.matrix(X_age_parfree)
    X_age_subspace <- as.matrix(X_age_subspace)
    n_freepar <- ncol(X_age_parfree)
    n_par <- nrow(X_age_parfree)
    n_age <- nrow(X_age_subspace)
    labels_age <- rownames(X_age_subspace)
    ## classifying variables
    draw <- rep(seq_len(n_draw), times = n_age)
    age <- rep(labels_age, each = n_draw)
    ## contribution from spline
    s <- seq.int(from = offset, length.out = n_freepar)
    val_parfree <- draws_all[ , s, drop = FALSE]
    val_par <- tcrossprod(val_parfree, X_age_parfree)
    ## contribution from line
    slope <- age_hyper$.value[age_hyper$hyper == "slope"]
    slope <- matrix(slope, nrow = n_draw, ncol = n_par)
    index <- seq.int(from = 0L, to = n_par - 1L) - 0.5 * (n_par - 1L)
    index <- matrix(index, nrow = n_draw, ncol = n_par, byrow = TRUE)
    val_line <- slope * index
    ## map from subspace to age effect
    val_subspace = val_par + val_line
    .value = tcrossprod(val_subspace, X_age_subspace)
    ## assemble and return
    .value <- as.numeric(.value)
    ans <- tibble::tibble(draw = draw,
                          age = age,
                          .value = .value)
    names(ans)[[2L]] <- agevar
    ans[[agevar]] <- tidy_agevar(x = ans[[agevar]],
                                   nm = agevar)
    ans
}


## HAS_TESTS
#' Construct draws of hyper-parameters from draws
#' of joint distribution of all unknowns
#'
#' @param draws_all A matrix with draws from the joint
#' distribution. Each row is one draw.
#' @param offset Index of first column of hyperparameters
#' @param spec Prior model for age or time effect.
#' Object of class "BayesRate_spec".
#'
#' @returns A matrix with same number of rows
#' as 'draws_all'.
#'
#' @noRd
make_draws_hyper <- function(draws_all, offset, spec) {
    n_draw <- nrow(draws_all)
    transforms <- get_transforms_hyper(spec)
    n_hyper <- length(transforms)
    draw <- rep(seq_len(n_draw), times = n_hyper)
    hyper <- rep(names(transforms), each = n_draw)
    s <- seq.int(from = offset, length.out = n_hyper)
    .value <- draws_all[ , s, drop = FALSE]
    for (i in seq_len(n_hyper))
        .value[, i] <- transforms[[i]](.value[, i])
    .value <- as.numeric(.value)
    tibble::tibble(draw = draw,
                   hyper = hyper,
                   .value = .value)
}


## HAS_TESTS
#' Construct draws of intercept from draws
#' of joint posterior of all unknowns
#'
#' @param draws_all A matrix with draws from the joint
#' distribution. Each row is one draw.
#'
#' @returns A tibble with columns 'draw' and 'intercept'
#'
#' @noRd
make_draws_intercept <- function(draws_all) {
    n_draw <- nrow(draws_all)
    draw <- seq_len(n_draw)
    .value <- draws_all[, 1L]
    tibble::tibble(draw = draw, 
                   .value = .value)
}


## HAS_TESTS
#' Extract and combine posterior draws from
#' multiple fitted models
#'
#' @param object An object of class "BayesRates_results",
#' created by a call to [smooth_age()] or
#' [smooth_agetime()].
#' @param n_draw Number of posterior draws
#'
#' @returns A named list of tibbles.
#'
#' @noRd
make_draws_post <- function(object, n_draw) {
    agevar <- object$agevar
    timevar <- object$timevar
    spec_time <- object$spec_time
    fitted <- object$fitted
    vals_by <- object$vals_by
    has_time_var <- has_time_var(spec_time)
    if (has_time_var)
        draws <- lapply(fitted,
                        make_draws_post_one_withtime,
                        agevar = agevar,
                        timevar = timevar,
                        n_draw = n_draw)
    else
        draws <- lapply(fitted,
                        make_draws_post_one_notime,
                        agevar = agevar,
                        n_draw = n_draw)
    ans <- merge_draws_with_vals_by(draws = draws,
                                    vals_by = vals_by)
    ans
}
    

## HAS_TESTS
#' Extract posterior draws from single fitted model
#' that does not include a time variable
#'
#' @param object An object of class "BayesRates_results",
#' created by a call to [smooth_age()].
#' @param agevar Name of age variable
#' @param n_draw Number of posterior draws
#'
#' @returns A named list of tibbles.
#'
#' @noRd
make_draws_post_one_notime <- function(fitted, agevar, n_draw) {
    mean <- fitted$mean
    prec <- fitted$prec
    spec_age <- fitted$spec_age
    X_age_parfree <- fitted$X_age_parfree
    X_age_subspace <- fitted$X_age_subspace
    draws_all <- rmvn(n = n_draw,
                      mean = mean,
                      prec = prec)
    intercept <- make_draws_intercept(draws_all)
    offset <- 2L
    age_hyper <- make_draws_hyper(draws_all = draws_all,
                                  offset = offset,
                                  spec = spec_age)
    offset <- offset + n_hyper(spec_age)
    age_effect <- make_draws_age_effect(draws_all = draws_all,
                                        offset = offset,
                                        X_age_parfree = X_age_parfree,
                                        X_age_subspace = X_age_subspace,
                                        agevar = agevar,
                                        age_hyper = age_hyper)
    rates <- combine_draws_effects_notime(intercept = intercept,
                                          age_effect = age_effect)
    list(rates = rates,
         intercept = intercept,
         age_effect = age_effect,
         age_hyper = age_hyper)
}


## HAS_TESTS
#' Extract posterior draws from single fitted model
#' that includes a time variable
#'
#' @param object An object of class "BayesRates_results",
#' created by a call to [smooth_agetime()].
#' @param agevar Name of the age variable
#' @param timevar Name of the time variable
#' @param n_draw Number of posterior draws
#'
#' @returns A named list of tibbles.
#'
#' @noRd
make_draws_post_one_withtime <- function(fitted, agevar, timevar, n_draw) {
    mean <- fitted$mean
    prec <- fitted$prec
    spec_age <- fitted$spec_age
    spec_time <- fitted$spec_time
    X_age_parfree <- fitted$X_age_parfree
    X_age_subspace <- fitted$X_age_subspace
    X_time <- fitted$X_time
    draws_all <- rmvn(n = n_draw,
                      mean = mean,
                      prec = prec)
    intercept <- make_draws_intercept(draws_all)
    offset <- 2L
    age_hyper <- make_draws_hyper(draws_all = draws_all,
                                  offset = offset,
                                  spec = spec_age)
    offset <- offset + n_hyper(spec_age)
    time_hyper <- make_draws_hyper(draws_all = draws_all,
                                   offset = offset,
                                   spec = spec_time)
    offset <- offset + n_hyper(spec_time)
    age_effect <- make_draws_age_effect(draws_all = draws_all,
                                        offset = offset,
                                        X_age_parfree = X_age_parfree,
                                        X_age_subspace = X_age_subspace,
                                        agevar = agevar,
                                        age_hyper = age_hyper)
    offset <- offset + ncol(as.matrix(X_age_parfree))
    time_effect <- make_draws_time_effect(draws_all = draws_all,
                                          offset = offset,
                                          spec_time = spec_time,
                                          agevar = agevar,
                                          timevar = timevar,
                                          X_age_subspace = X_age_subspace,
                                          X_time = X_time)
    rates <- combine_draws_effects_withtime(intercept = intercept,
                                            age_effect = age_effect,
                                            time_effect = time_effect)
    list(rates = rates,
         intercept = intercept,
         age_effect = age_effect,
         time_effect = time_effect,
         age_hyper = age_hyper,
         time_hyper = time_hyper)
}


## HAS_TESTS
#' Construct draws of time effect or age-time interaction
#' from draws of joint posterior of all unknowns
#'
#' @param draws_all A matrix with draws from the joint
#' distribution. Each row is one draw.
#' @param offset Index of first column of age effect
#' @param spec_time Object of class "BayesRates_spec"
#' specifying the time or age-time effect
#' @param agevar Name of age variable
#' @param timevar Name of time variable
#' @param X_age_subspace Matrix mapping subspace parameters to age effect
#' @param X_time Matrix mapping free parameters to time effect
#'
#' @returns A tibble with the following columns:
#' - If 'spec_time' specifies a time main effect:
#' 'draw', timevar, '.value'
#' - If 'spec_time' specifies an age-time interaction:
#' 'draw', agevar, timevar, '.value'
#'
#' @noRd
make_draws_time_effect <- function(draws_all,
                                   offset,
                                   spec_time,
                                   agevar,
                                   timevar,
                                   X_age_subspace,
                                   X_time) {
    n_draw <- nrow(draws_all)
    X_time <- as.matrix(X_time)
    n_time <- nrow(X_time)
    n_parfree_time <- ncol(X_time)
    labels_time <- rownames(X_time)
    labels_time <- as.integer(labels_time)
    is_interaction <- is_interaction(spec_time)
    if (is_interaction) {
        X_age_subspace <- as.matrix(X_age_subspace)
        n_age <- nrow(X_age_subspace)
        labels_age <- rownames(X_age_subspace)
        s <- seq.int(from = offset,
                     length.out = n_age * n_parfree_time)
        draw <- rep(seq_len(n_draw), times = n_age * n_time)
        age <- rep(rep(labels_age, each = n_draw), times = n_time)
        time <- rep(labels_time, each = n_draw * n_age)
        .value <- draws_all[, s, drop = FALSE]
        .value <- matrix(.value,
                        nrow = n_draw * n_age,
                        ncol = n_parfree_time)
        .value <- .value %*% t(X_time)
        .value <- as.numeric(.value)
        ans <- tibble(draw = draw,
                      age = age,
                      time = time,
                      .value = .value)
        names(ans)[[2L]] <- agevar
        names(ans)[[3L]] <- timevar
        ans[[agevar]] <- tidy_agevar(x = ans[[agevar]],
                                       nm = agevar)
    }
    else {
        s <- seq.int(from = offset,
                     length.out = n_parfree_time)
        draw <- rep(seq_len(n_draw), times = n_time)
        time <- rep(labels_time, each = n_draw)
        .value <- draws_all[ , s, drop = FALSE]
        .value <- .value %*% t(X_time)
        .value <- as.numeric(.value)
        ans <- tibble(draw = draw,
                      time = time,
                      .value = .value)
        names(ans)[[2L]] <- timevar
    }
    ans
}


#' Fit model
#'
#' Workhorse function for `smooth_agetime()`.
#' Given matrices of events and
#' person-years at risk, plus prior
#' models for age and time, approximate the
#' posterior distribution.
#'
#' @param nevent A matrix of counts of events by age by time
#' @param py A matrix of person-years of exposure, by age and time
#' @param spec_age An object of class `"BayesRates_spec"`
#' describing the prior model for age effects
#' @param spec_time An object of class `"BayesRates_spec"`
#' describing the prior model for time effects
#' 
#' @returns Named list with elements
#' "mean", "var", "spec_age", "spec_time",
#' "X_age_parfree", and "X_age_subspace"
#'
#' @noRd
make_fitted <- function(nevent, py, spec_age, spec_time) {
    labels_age <- rownames(nevent)
    labels_time <- colnames(nevent)
    n_age <- length(labels_age)
    n_time <- length(labels_time)
    ## assemble data
    is_in_lik <- 1L * (!is.na(nevent) & !is.na(py))
    class_spec_age <- sub("^BayesRates_", "", class(spec_age)[[1L]])
    class_spec_time <- sub("^BayesRates_", "", class(spec_time)[[1L]])
    X_age_parfree <- make_X_age_parfree(spec = spec_age,
                                        labels_age = labels_age)
    X_age_subspace <- make_X_age_subspace(spec = spec_age,
                                          labels_age = labels_age)
    X_time <- make_X_time(spec = spec_time,
                          labels_time = labels_time)
    scale_age <- get_scale(spec_age)
    scale_time <- get_scale(spec_time)
    data <- list(nevent = nevent,
                 py = py,
                 is_in_lik = is_in_lik,
                 class_spec_age = class_spec_age,
                 class_spec_time = class_spec_time,
                 X_age_parfree = X_age_parfree,
                 X_age_subspace = X_age_subspace,
                 X_time = X_time,
                 scale_age = scale_age,
                 scale_time = scale_time)
    ## assemble parameters
    parfree_age <- make_parfree_age(spec = spec_age,
                                    labels_age = labels_age)
    parfree_time <- make_parfree_time(spec = spec_time,
                                      labels_age = labels_age,
                                      labels_time = labels_time)
    parameters = list(intercept = -1,
                      log_sd_age = 0,
                      slope_age = 0,
                      log_sd_time = 0,
                      logit_rho_time = 0.8,
                      parfree_age = parfree_age,
                      parfree_time = parfree_time)
    map <- make_map(spec_time)
    random <- make_random(spec_time)
    ## optimisation
    f <- TMB::MakeADFun(data = data,
                        parameters = parameters,
                        DLL = "BayesRates",
                        map = map,
                        random = random,
                        silent = TRUE)
    suppressWarnings(
        convergence <- stats::nlminb(start = f$par,
                                     objective = f$fn,
                                     gradient = f$gr)
    )
    ## record results
    sdreport <- TMB::sdreport(f,
                              bias.correct = TRUE,
                              getJointPrecision = TRUE)
    mean <- c(sdreport$par.fixed, sdreport$par.random)
    prec <- sdreport$jointPrecision
    list(mean = mean,
         prec = prec,
         spec_age = spec_age,
         spec_time = spec_time,
         X_age_parfree = X_age_parfree,
         X_age_subspace = X_age_subspace,
         X_time = X_time,
         convergence = convergence)
}


#' Given a credible interval width,
#' make the 'probs' argument
#' to be supplied to quantiles function
#'
#' @param interval A number between 0 and 1.
#'
#' @returns A numeric vector of length 3.
#'
#' @noRd
make_probs <- function(interval) {
    checkmate::assert_number(interval, lower = 0, upper = 1)
    alpha <- 1 - interval
    c(0.5 * alpha, 0.5, 1 - 0.5 * alpha)
}


## HAS_TESTS
#' Matrix to first order random walk
#'
#' @param n Number of elements of random walk vector.
#'
#' @returns Vector of length n
#'
#' @noRd
make_rw_matrix <- function(n) {
    D <- make_diff_matrix(n)
    DD <- Matrix::tcrossprod(D)
    DD_inv <- Matrix::solve(DD)
    Matrix::crossprod(D, DD_inv)
}


## HAS_TESTS
#' Matrix to create second order random walk
#'
#' @param n Number of elements of random walk vector.
#'
#' @returns Vector of length n
#'
#' @noRd
make_rw2_matrix <- function(n) {
    D1 <- make_diff_matrix(n)
    D2 <- make_diff_matrix(n - 1L)
    M <- D2 %*% D1
    MM <- Matrix::tcrossprod(M)
    MM_inv <- Matrix::solve(MM)
    Matrix::crossprod(M, MM_inv)
}


## HAS_TESTS
#' Make a matrix of B-spline basis functions
#'
#' Based on Eilers and Marx (1996). Flexible Smoothing
#' with B-splines and Penalties.
#' Statistical Science, 11(2), 89-121.
#'
#' @param n Number of elements vector being modelled
#' @param df Degrees of freedom
#'
#' @returns Matrix with n rows and df columns
#'
#' @noRd
make_spline_matrix <- function(n, df) {
    n_interval <- df - 3L
    interval_length <- (n - 1L) / n_interval
    start <- 1 - 3 * interval_length
    end <- n + 3 * interval_length
    x <- seq(from = start, to = end, by = 0.001)
    base <- splines::bs(x = x, df = df + 5L)
    i_keep <- findInterval(seq_len(n), x)
    j_keep <- seq.int(from = 3L, length.out = df)
    ans <- base[i_keep, j_keep]
    colmeans <- colMeans(ans)
    ans <- ans - rep(colmeans, each = nrow(ans))
    Matrix::sparseMatrix(i = row(ans),
                         j = col(ans),
                         x = as.double(ans))
}


## HAS_TESTS
#' Store values of 'by' variable(s) in form
#' that is convenient for merging with
#' posterior draws
#'
#' @param df Data frame created by merging
#' nevent_df and py_df
#' @param byvar Name(s) of 'by' variables.
#' Can have length 0.
#'
#' @returns A list, possibly empty.
#'
#' @noRd
make_vals_by <- function(df, byvar) {
    if (length(byvar) > 0L) {
        ans <- split(df[byvar], df[byvar])
        ans <- lapply(ans, unique)
        ans <- lapply(ans, tibble::tibble)
    }
    else
        ans <- list()
    ans
}
    

## HAS_TESTS
#' Merge 'by' variables into posterior draws and rbind results
#'
#' @param draws_post List of lists. Each element of the inner
#' list is a set of posterior samples for a particular combination
#' of 'by' variables.
#' @param data List of data frames used as input for fitting.
#'
#' @return A list of tibbles.
#'
#' @noRd
merge_draws_with_vals_by <- function(draws, vals_by) {
    n_by <- length(vals_by)
    has_by <- n_by > 0L
    if (has_by) {
        nms_elements <- names(draws[[1L]])
        n_element <- length(nms_elements)
        ## create list of length n_element x n_by
        ans <- .mapply(merge,
                       dots = list(x = rep(vals_by, each = n_element),
                                   y = unlist(draws, recursive = FALSE)),
                       MoreArgs = list())
        ## turn into matrix with dim c(n_by, n_element), using byrow = TRUE
        ans <- matrix(ans, nrow = n_by, ncol = n_element, byrow = TRUE)
        ## rbind columns
        ans <- apply(ans, 2L, function(args) do.call(rbind, args), simplify = FALSE)
        ans <- lapply(ans, tibble::tibble)
        names(ans) <- nms_elements
    }
    else
        ans <- draws[[1L]]
    ans
}


## HAS_TESTS    
#' Reformat age variable so it has same class as target
#'
#' @param var_current Age variable to be reformatted
#' @param var_target Age variable to emulate
#' @param agevar Name of age variable
#'
#' @returns Reformatted version of 'var_current'
#'
#' @noRd    
reformat_var_age <- function(var_current, var_target, agevar) {
    if (is.integer(var_target))
        as.integer(var_current)
    else if (is.numeric(var_target))
        as.numeric(var_current)
    else if (is.factor(var_target))
        factor(var_current, levels = levels(var_target))
    else if (is.character(var_target))
        as.character(var_current)
    else
        stop(gettextf("'%s' has class \"%s\"",
                      agevar,
                      class(var_target)),
             call. = FALSE)
}


## HAS_TESTS
#' Reformat time variable so it has same class as target
#'
#' @param var_current Time variable to be reformatted
#' @param var_target Time variable to emulate
#' @param timevar Name of time variable
#'
#' @returns Reformatted version of 'var_current'
#'
#' @noRd    
reformat_var_time <- function(var_current, var_target, timevar) {
    if (is.integer(var_target))
        as.integer(var_current)
    else if (is.numeric(var_target))
        as.numeric(var_current)
    else
        stop(gettextf("'%s' has class \"%s\"",
                      timevar,
                      class(var_target)),
             call. = FALSE)
}


## HAS_TESTS
#' Draw from a multivariate normal distribution
#'
#' Code based partly on MASS::mvrnorm.
#' 
#' @param n Number of draws
#' @param mean Mean of MVN distribution
#' @param prec Precision of MVN distribution
#'
#' @returns Matrix with length(mean)
#' rows and n columns.
#'
#' @noRd
rmvn <- function(n, mean, prec) {
    n_val <- length(mean)
    ch <- chol(prec)
    I <- diag(n_val)
    sd <- backsolve(ch, I)
    Z <- matrix(stats::rnorm(n = n_val * n),
                nrow = n_val,
                ncol = n)
    t(mean + sd %*% Z)
}


## HAS_TESTS
#' Coerce age variable to correct format
#'
#' Coerce to integer if possible.
#' Otherwise, leave age variable untouched if it
#' is numeric or a factor, and coerce to
#' a factor (with levels ordered by appearance)
#' if it is character.
#'
#' @param x The age variable
#' @param nm Name of age variable
#'
#' @returns Vector with same length as 'x'
#'
#' @noRd
tidy_agevar <- function(x, nm) {
    if (is.numeric(x)) {
        x
    }
    else if (is.factor(x)) {
        if (is_char_int(x)) {
            x <- as.character(x)
            as.integer(x)
        }
        else {
            x
        }
    }
    else if (is.character(x)) {
        if (is_char_int(x)) {
            as.integer(x)
        }
        else {
            factor(x, levels = unique(x))
        }
    }
    else {
        stop(gettextf("'%s' has class \"%s\"",
                      nm,
                      class(x)),
             call. = FALSE)
    }
}


## HAS_TESTS
#' Coerce time variable to correct format
#'
#' Coerce to integer if character or factor.
#' A check to make sure this is possible
#' should already have been run.
#'
#' @param x The time variable
#' @param nm Name of time variable
#'
#' @returns Vector with same length as 'x'
#'
#' @noRd
tidy_timevar <- function(x, nm) {
    if (is.numeric(x)) {
        x
    }
    else if ((is.factor(x) || is.character(x)) && is_char_int(x)) {
        x <- as.character(x)
        as.integer(x)
    }
    else {
        stop(gettextf("'%s' has class \"%s\"",
                      nm,
                      class(x)),
             call. = FALSE)
    }
}
    
