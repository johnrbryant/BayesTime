
## HAS_TESTS
#' Combine posterior samples for intercept, age effect,
#' and time effect to obtain a posterior sample for
#' log rates
#'
#' In 'age_effect', 'time_effect', and the return
#' value, each row holds one draw.
#'
#' @param intercept Vector containing posterior sample
#' for intercept
#' @param age_effect Matrix containing posterior sample
#' for age effect
#' @param time_effect Matrix containing posterior sample
#' for time effect
#'
#' @returns An array with dimensions 'draw', agevar, and timevar
#'
#' @noRd
combine_draws_effects <- function(intercept, age_effect, time_effect) {
    n_draw <- length(intercept)
    n_age <- ncol(age_effect)
    n_time <- ncol(time_effect)
    dn_age <- dimnames(age_effect)[2L]
    dn_time <- dimnames(time_effect)[2L]
    agevar <- names(dn_age)
    timevar <- names(dn_time)
    age_labels <- dn_age[[1L]]
    time_labels <- dn_time[[1L]]
    intercept <- rep(intercept, times = n_age * n_time)
    age_effect <- rep(age_effect, times = n_time)
    time_effect <- apply(time_effect, 2L, rep, times = n_age)
    ans <- intercept + age_effect + time_effect
    ans <- array(ans,
                 dim = c(n_draw, n_age, n_time),
                 dimnames = list(seq_len(n_draw),
                                 age_labels,
                                 time_labels))
    names(dimnames(ans)) <- c("draw", agevar, timevar)
    ans
}


## HAS_TESTS
#' Coerce age variable to correct format
#'
#' Leave age variable untouched if it
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
format_agevar <- function(x, nm) {
    if (is.numeric(x))
        x
    else if (is.factor(x))
        x
    else if (is.character(x))
        factor(x, levels = unique(x))
    else
        stop(gettextf("'%s' has class \"%s\"",
                      nm,
                      class(x)),
             call. = FALSE)
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
#' Make matrix holding event or person-year data by age by time
#'
#' @param data Data frame with age, time, and measurement variables
#' @param measurevar Name of measurement variable
#' @param agevar Name of age variable
#' @param timevar Name of time variable
#'
#' @returns An array
#'
#' @noRd
make_agetime_matrix <- function(data, measurevar, agevar, timevar) {
    formula <- sprintf("%s ~ %s + %s", measurevar, agevar, timevar)
    formula <- stats::as.formula(formula)
    ans <- stats::xtabs(formula, data = data, addNA = TRUE)
    array(ans, dim = dim(ans), dimnames = dimnames(ans))
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
#' Construct draws of age effect from draws
#' of joint posterior of all unknowns
#'
#' @param draws_all A matrix with draws from the joint
#' distribution. Each row is one draw.
#' @param offset Index of first column of age effect
#' @param model Prior model for age effect.
#' Object of class "BayesRate_model"
#' @param agevar Name of age variable
#'
#' @returns A matrix with same number of rows
#' as 'draws_all'.
#'
#' @noRd
make_draws_age_effect <- function(draws_all, offset, X_age, agevar) {
    n_draw <- nrow(draws_all)
    X_age <- as.matrix(X_age)
    n_freepar <- ncol(X_age)
    s <- seq.int(from = offset, length.out = n_freepar)
    ans <- draws_all[ , s, drop = FALSE]
    ans <- ans %*% t(X_age)
    rownames(ans) <- seq_len(n_draw)
    colnames(ans) <- rownames(X_age)
    names(dimnames(ans)) <- c("draw", agevar)
    ans
}


## HAS_TESTS
#' Construct draws of hyper-parameters from draws
#' of joint distribution of all unknowns
#'
#' @param draws_all A matrix with draws from the joint
#' distribution. Each row is one draw.
#' @param offset Index of first column of hyperparameters
#' @param model Prior model for age or time effect.
#' Object of class "BayesRate_model".
#'
#' @returns A matrix with same number of rows
#' as 'draws_all'.
#'
#' @noRd
make_draws_hyper <- function(draws_all, offset, model) {
    n_draw <- nrow(draws_all)
    transforms <- get_transforms_hyper(model)
    n_hyper <- length(transforms)
    s <- seq.int(from = offset, length.out = n_hyper)
    ans <- draws_all[ , s, drop = FALSE]
    for (i in seq_len(n_hyper))
        ans[, i] <- transforms[[i]](ans[, i])
    colnames(ans) <- names(transforms)
    rownames(ans) <- seq_len(n_draw)
    names(dimnames(ans)) <- c("draw", "hyper")
    ans
}


## HAS_TESTS
#' Construct draws of intercept from draws
#' of joint posterior of all unknowns
#'
#' @param draws_all A matrix with draws from the joint
#' distribution. Each row is one draw.
#'
#' @returns A matrix with one column and with
#' same number of rows as 'draws_all'.
#'
#' @noRd
make_draws_intercept <- function(draws_all) {
    n_draw <- nrow(draws_all)
    ans <- draws_all[, 1L, drop = FALSE]
    colnames(ans) <- "(Intercept)"
    rownames(ans) <- seq_len(n_draw)
    names(dimnames(ans)) <- c("draw", "(Intercept)")
    ans
}


## HAS_TESTS
#' Construct draws of time effect from draws
#' of joint posterior of all unknowns
#'
#' @param draws_all A matrix with draws from the joint
#' distribution. Each row is one draw.
#' @param offset Index of first column of age effect
#' @param nevent Matrix of events by age by time.
#' @param timevar Name of time variable
#'
#' @returns A matrix with same number of rows
#' as 'draws_all'.
#'
#' @noRd
make_draws_time_effect <- function(draws_all, offset, timevar) {
    n_draw <- nrow(draws_all)
    n_col <- ncol(draws_all)
    s <- seq.int(from = offset, to = n_col)
    ans <- draws_all[ , s, drop = FALSE]
    rownames(ans) <- seq_len(n_draw)
    colnames(ans) <- sub("^effect\\.", "", colnames(ans))
    names(dimnames(ans)) <- c("draw", timevar)
    ans
}



#' Fit model
#'
#' Workhorse function for `smooth.agetime()`.
#' Given matrices of events and
#' person-years at risk, plus prior
#' models for age and time, approximate the
#' posterior distribution.
#'
#' @param nevent A matrix of counts of events by age by time
#' @param py A matrix of person-years of exposure, by age and time
#' @param model_age An object of class `"BayesRates_model"`
#' describing the prior model for age effects
#' @param model_time An object of class `"BayesRates_model"`
#' describing the prior model for time effects
#' 
#' @returns Named list with elements
#' "mean", "var", "model_age",
#' "model_time", and "X_age".
#'
#' @noRd
make_fitted <- function(nevent, py, model_age, model_time) {
    labels_age <- rownames(nevent)
    labels_time <- colnames(nevent)
    labels_age <- rownames(nevent)
    class_model_age <- class(model_age)[[1L]]
    class_model_time <- class(model_time)[[1L]]
    X_age <- get_X_age(model = model_age,
                       labels_age)
    consts_age <- get_consts(model_age)
    consts_time <- get_consts(model_time)
    data <- list(nevent = nevent,
                 py = py,
                 class_model_age = class_model_age,
                 class_model_time = class_model_time,
                 X_age = X_age,
                 consts_age = consts_age,
                 consts_time = consts_time)
    ## parameters
    par_age <- get_par(model = model_age, labels = labels_age)
    par_time <- get_par(model = model_time, labels = labels_time)
    parameters = list(intercept = 0,
                      par_age = par_age,
                      par_time = par_time)
    ## optimisation
    f <- TMB::MakeADFun(data = data,
                        parameters = parameters,
                        DLL = "BayesRates",
                        ## hessian = TRUE,
                        silent = TRUE)
    suppressWarnings(
        stats::nlminb(start = f$par,
                  objective = f$fn,
                  gradient = f$gr,
                  hessian = f$he)
    )
    ## record results
    rep <- TMB::sdreport(f, getReportCovariance = TRUE)
    mean <- rep$par.fixed
    var <- rep$cov.fixed
    nms <- c("intercept", names(par_age), names(par_time))
    names(mean) <- nms
    dimnames(var) <- list(nms, nms)
    list(mean = mean,
         var = var,
         model_age = model_age,
         model_time = model_time,
         X_age = X_age)
}


#' Extract posterior draws from fitted model
#'
#' @param fitted A named list. Output from
#' function 'smooth_agetime_inner'
#' @param n_draw Number of posterior draws
#' @param nevent_df Data frame containing
#' data on events
#' @param agevar Name of age variable
#' @param timevar Name of time variable
#'
#' @returns A named list of tibbles.
#'
#' @noRd
make_post_draws <- function(fitted, n_draw, nevent_df, agevar, timevar) {
    mean <- fitted$mean
    var <- fitted$var
    model_age <- fitted$model_age
    model_time <- fitted$model_time
    X_age <- fitted$X_age
    draws_all <- MASS::mvrnorm(n = n_draw,
                               mu = mean,
                               Sigma = var,
                               tol = Inf)
    intercept <- make_draws_intercept(draws_all)
    offset <- 1L + ncol(intercept)
    age_hyper <- make_draws_hyper(draws_all = draws_all,
                                  offset = offset,
                                  model = model_age)
    offset <- offset + ncol(age_hyper)
    age_effect <- make_draws_age_effect(draws_all = draws_all,
                                        offset = offset,
                                        X_age = X_age,
                                        agevar = agevar)
    offset <- offset + ncol(age_effect)
    time_hyper <- make_draws_hyper(draws_all = draws_all,
                                   offset = offset,
                                   model = model_time)
    offset <- offset + ncol(time_hyper)
    time_effect <- make_draws_time_effect(draws_all = draws_all,
                                          offset = offset,
                                          timevar = timevar)
    log_rates <- combine_draws_effects(intercept = intercept,
                                       age_effect = age_effect,
                                       time_effect = time_effect)
    rates <- exp(log_rates)
    rates <- reformat_rates(x = rates,
                            df = nevent_df,
                            agevar = agevar,
                            timevar = timevar)
    intercept <- reformat_intercept(intercept)
    age_effect <- reformat_age_effect(x = age_effect,
                                      df = nevent_df,
                                      agevar = agevar)
    time_effect <- reformat_time_effect(x = time_effect,
                                        df = nevent_df,
                                        timevar = timevar)
    age_hyper <- reformat_array(age_hyper)
    time_hyper <- reformat_array(time_hyper)
    list(rates = rates,
         intercept = intercept,
         age_effect = age_effect,
         time_effect = time_effect,
         age_hyper = age_hyper,
         time_hyper = time_hyper)
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
    m1 <- make_accum_matrix(n - 1L)   ## n-1 x n-2
    m2 <- make_center_matrix(n - 1L)  ## n-1 x n-1
    m3 <- make_accum_matrix(n)        ## n x n-1
    m4 <- make_center_matrix(n)       ## n x n
    m4 %*% m3 %*% m2 %*% m1           ## n x n-2
}


## HAS_TESTS
#' Make a matrix of spline basis functions
#'
#' @param n Number of elements vector being modelled
#' @param df Degrees of freedom
#'
#' @returns Matrix with n rows and df columns
#'
#' @noRd
make_spline_matrix <- function(n, df) {
    x <- seq_len(n)
    ans <- splines::bs(x = x, df = df)
    nr <- nrow(ans)
    nc <- ncol(ans)
    i <- rep(seq.int(from = 2L, to = nr), times = nc)
    j <- rep(seq_len(nc), each = nr - 1L)
    xx <- as.numeric(ans[-1, ])
    Matrix::sparseMatrix(i = i, j = j, x = xx)
}


## HAS_TESTS
#' Merge 'by' variables into posterior draws and rbind results
#'
#' @param post_draws List of lists. Each element of the inner
#' list is a set of posterior samples for a particular combination
#' of 'by' variables.
#' @param data List of data frames used as input for fitting.
#' @param byvar Names of 'by' variables. Can have zero length.
#'
#' @return A list of tibbles.
#'
#' @noRd
merge_byvar_with_post <- function(post_draws, data, byvar) {
    has_by <- length(byvar) > 0L
    if (has_by) {
        by <- lapply(data, function(x) unique(x[byvar])) ## split in same way as 'data' arg
        n_by <- length(by)
        nms_elements <- names(post_draws[[1L]])
        n_element <- length(nms_elements)
        ans <- .mapply(merge,
                       dots = list(x = rep(by, each = n_element),
                                   y = unlist(post_draws, recursive = FALSE)),
                       MoreArgs = list())
        ans <- matrix(ans, nrow = n_by, ncol = n_element, byrow = TRUE)
        ans <- apply(ans, 2L, function(args) do.call(rbind, args), simplify = FALSE)
        ans <- lapply(ans, tibble::tibble)
        names(ans) <- nms_elements
        ans
    }
    else
        post_draws[[1L]]
}


## HAS_TESTS
#' Reformat array containing draws from posterior distribution
#'
#' Convert array to tibble with measure variable "value",
#' and with integer values for 'draw'.
#'
#' @param x An array (including matrices)
#'
#' @returns A tibble
#'
#' @noRd
reformat_array <- function(x) {
    x <- as.data.frame.table(x,
                             responseName = "value",
                             stringsAsFactors = FALSE)
    x[["draw"]] <- as.integer(x[["draw"]])
    x <- tibble::tibble(x)
    x
}


## HAS_TESTS
#' Reformat array containing draws from posterior distribution
#' for age effect
#'
#' @param x A matrix with draw and age dimensions
#' @param df A data frame with age variable in the required
#' format
#' @param agevar Name of the age variable
#'
#' @returns A tibble
#'
#' @noRd
reformat_age_effect <- function(x, df, agevar) {
    x <- reformat_array(x)
    x[[agevar]] <- reformat_var_age(var_current = x[[agevar]],
                                    var_target = df[[agevar]],
                                    agevar = agevar)
    x
}


## HAS_TESTS
#' Reformat array containing draws from posterior distribution
#' for intercept
#'
#' @param x A matrix with draw dimension
#'
#' @returns A tibble
#'
#' @noRd
reformat_intercept <- function(x) {
    x <- reformat_array(x)
    x <- x[c("draw", "value")]
    x
}


## HAS_TESTS
#' Reformat array containing draws from posterior distribution
#' for rates
#'
#' @param x An array with draw, age, and time dimensions
#' @param df A data frame with variables in the required
#' format
#' @param agevar Name of the age variable
#' @param timevar Name of the time variable
#'
#' @returns A tibble
#'
#' @noRd
reformat_rates <- function(x, df, agevar, timevar) {
    x <- reformat_array(x)
    x[[agevar]] <- reformat_var_age(var_current = x[[agevar]],
                                    var_target = df[[agevar]],
                                    agevar = agevar)
    x[[timevar]] <- reformat_var_time(var_current = x[[timevar]],
                                      var_target = df[[timevar]],
                                      timevar = timevar)
    x
}


## HAS_TESTS
#' Reformat array containing draws from posterior distribution
#' for time effect
#'
#' @param x A matrix with draw and time dimensions
#' @param df A data frame with time variable in the required
#' format
#' @param timevar Name of the time variable
#'
#' @returns A tibble
#'
#' @noRd
reformat_time_effect <- function(x, df, timevar) {
    x <- reformat_array(x)
    x[[timevar]] <- reformat_var_time(var_current = x[[timevar]],
                                      var_target = df[[timevar]],
                                      timevar = timevar)
    x
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
