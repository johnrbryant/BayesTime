

## 'combine_draws_effects_notime' ---------------------------------------------

test_that("'combine_draws_effects_notime' works with valid input", {
    set.seed(0)
    intercept <- tibble(draw = 1:20,
                        .value = rnorm(20))
    age_effect <- tibble(draw = rep(1:20, times = 4),
                         Age = rep(c("0-4", "5-9", "10-14", "15-19"), each = 20),
                         .value = rnorm(80))
    ans_obtained <- combine_draws_effects_notime(intercept = intercept,
                                                 age_effect = age_effect)
    ans_expected <- merge(intercept, age_effect, by = "draw")
    value <- with(ans_expected, exp(.value.x + .value.y))
    ans_expected <- ans_expected[c("draw", "Age")]
    ans_expected$.value <- value
    ans_expected <- ans_expected[with(ans_expected, order(draw, Age)), ]
    ans_expected <- tibble(ans_expected)
    expect_identical(ans_obtained, ans_expected)
})


## 'combine_draws_effects_withtime' -------------------------------------------

test_that("'combine_draws_effects_withtime' works with valid input - time main effect", {
    set.seed(0)
    intercept <- tibble(draw = 1:20,
                            .value = rnorm(20))
    age_effect <- tibble(draw = rep(1:20, times = 4),
                             Age = rep(c("0-4", "5-9", "10-14", "15-19"), each = 20),
                             .value = rnorm(80))
    time_effect <- tibble(draw = rep(1:20, times = 5),
                              Time = rep(2001:2005, each = 20),
                              .value = rnorm(100))
    ans_obtained <- combine_draws_effects_withtime(intercept = intercept,
                                          age_effect = age_effect,
                                          time_effect = time_effect)
    ans_expected <- merge(merge(intercept, age_effect, by = "draw"), time_effect, by = "draw")
    value <- with(ans_expected, exp(.value.x + .value.y + .value))
    ans_expected <- ans_expected[c("draw", "Age", "Time")]
    ans_expected$.value <- value
    ans_expected <- ans_expected[with(ans_expected, order(draw, Age, Time)), ]
    ans_expected <- tibble(ans_expected)
    expect_identical(ans_obtained, ans_expected)
})

test_that("'combine_draws_effects_withtime' works with valid input - age-time interaction", {
    set.seed(0)
    intercept <- tibble(draw = 1:20,
                            .value = rnorm(20))
    age_effect <- tibble(draw = rep(1:20, times = 4),
                             Age = rep(c("0-4", "5-9", "10-14", "15-19"), each = 20),
                             .value = rnorm(80))
    time_effect <- tibble(draw = rep(1:20, times = 20),
                          Age = rep(rep(c("0-4", "5-9", "10-14", "15-19"), each = 20), times = 5),
                          Time = rep(2001:2005, each = 80),
                          .value = rnorm(400))
    ans_obtained <- combine_draws_effects_withtime(intercept = intercept,
                                          age_effect = age_effect,
                                          time_effect = time_effect)
    ans_expected <- merge(merge(intercept, age_effect, by = "draw"), time_effect, by = c("draw", "Age"))
    value <- with(ans_expected, exp(.value.x + .value.y + .value))
    ans_expected <- ans_expected[c("draw", "Age", "Time")]
    ans_expected$.value <- value
    ans_expected <- ans_expected[with(ans_expected, order(draw, Age, Time)), ]
    ans_expected <- tibble(ans_expected)
    expect_identical(ans_obtained, ans_expected)
})


## 'format_agevar' ------------------------------------------------------------

test_that("'format_agevar' works with valid input", {
    expect_identical(format_agevar(1:3, "age"), 1:3)
    expect_identical(format_agevar(factor(c("a", "b")), "age"), factor(c("a", "b")))
    expect_identical(format_agevar(c("b", "a", "b"), "age"),
                     factor(c("b", "a", "b"), levels = c("b", "a")))
})
    
test_that("'format_agevar' raises correct error with invalid input", {
    expect_error(format_agevar(NULL, "age"),
                 "'age' has class \"NULL\"")
})


## 'make_accum_matrix' --------------------------------------------------------

test_that("'make_accum_matrix' works", {
    set.seed(0)
    n <- 10
    m <- make_accum_matrix(n)
    x <- rnorm(n - 1)
    y <- as.numeric(m %*% x)
    expect_equal(diff(y), x)
})


## 'make_age_matrix' ----------------------------------------------------------

test_that("'make_age_matrix' works", {
    data <- expand.grid(region = c("a", "b"), age = 0:1,
                        KEEP.OUT.ATTRS = FALSE)
    data$nevent <- 1
    ans_obtained <- make_age_matrix(data,
                                    measurevar = "nevent",
                                    agevar = "age")
    ans_expected <- matrix(2, nrow = 2, ncol = 1,
                           dimnames = list(age = 0:1, NULL))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_agetime_matrix' ------------------------------------------------------

test_that("'make_agetime_matrix' works", {
    data <- expand.grid(time = 2000:2002, age = 0:1,
                        KEEP.OUT.ATTRS = FALSE)
    data$nevent <- 1
    ans_obtained <- make_agetime_matrix(data,
                                        measurevar = "nevent",
                                        agevar = "age",
                                        timevar = "time")
    ans_expected <- matrix(1, nrow = 2, ncol = 3,
                           dimnames = list(age = 0:1, time = 2000:2002))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_center_matrix' -------------------------------------------------------

test_that("'make_center_matrix' works", {
    set.seed(0)
    n <- 10
    m <- make_center_matrix(n)
    x <- rnorm(n)
    y <- as.numeric(m %*% x)
    expect_equal(sum(y), 0)
})


## 'make_credible_intervals' --------------------------------------------------

test_that("'make_credible_intervals' works - 'x' has classif vars", {
    set.seed(0)
    x <- expand.grid(age = 0:4,
                     time = 2001:2003,
                     sex = c("F", "M"),
                     draw = 1:20,
                     KEEP.OUT.ATTRS = FALSE)
    x$.value <- rnorm(n = nrow(x))
    ans <- make_credible_intervals(x, measurevar = ".value", width = 0.9)
    lower <- aggregate(x$.value, x[c("age", "time", "sex")], quantile, prob = 0.05)
    lower <- merge(ans, lower)
    expect_equal(lower$.lower, lower$x)
    mid <- aggregate(x$.value, x[c("age", "time", "sex")], quantile, prob = 0.5)
    mid <- merge(ans, mid)
    expect_equal(mid$.fitted, mid$x)
    upper <- aggregate(x$.value, x[c("age", "time", "sex")], quantile, prob = 0.95)
    upper <- merge(ans, upper)
    expect_equal(upper$.upper, upper$x)
})

test_that("'make_credible_intervals' works - 'x' does not have classif vars", {
    set.seed(0)
    x <- data.frame(draw = 1:100,
                    .value = rnorm(100))
    ans_obtained <- make_credible_intervals(x, measurevar = ".value", width = 0.9)
    ans_expected <- tibble(.fitted = median(x$.value),
                           .lower = as.numeric(quantile(x$.value, 0.05)),
                           .upper = as.numeric(quantile(x$.value, 0.95)))
    expect_equal(ans_obtained, ans_expected)
})

## 'make_diff_matrix' ---------------------------------------------------------

test_that("'make_diff_matrix' works", {
    set.seed(0)
    n <- 10
    m <- make_diff_matrix(n)
    x <- rnorm(n)
    expect_equal(as.numeric(m %*% x), diff(x))
})


## 'make_draws_age_effect' ----------------------------------------------------

test_that("'make_draws_age_effect' works", {
    draws_all <- matrix((1:200)/100, nc = 10)
    offset <- 2L
    model <- RW2()
    labels_age <- c("0", "1", "2", "3+")
    X_age_parfree <- make_X_age_parfree(model,  labels_age = labels_age)
    X_age_subspace <- make_X_age_subspace(model,  labels_age = labels_age)
    age_hyper <- data.frame(draw = c(1:20, 1:20),
                            hyper = rep(c("sd", "slope"), each = 20),
                            .value = c(rep(0.1, 20), draws_all[,1]))
    ans_obtained <- make_draws_age_effect(draws_all = draws_all,
                                          offset = offset,
                                          X_age_parfree = X_age_parfree,
                                          X_age_subspace = X_age_subspace,
                                          agevar = "Age",
                                          age_hyper = age_hyper)
    val_par <- as.numeric(draws_all[,2:3] %*% t(as.matrix(X_age_parfree)))
    val_line <- rep(draws_all[,1], times = 4) * rep((0:3) - 1.5, each = 20)
    ans_expected <- tibble(draw = rep(1:20, 4),
                           Age = factor(rep(labels_age, each = 20), levels = labels_age),
                           .value = (val_par + val_line))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_draws_intercept' -----------------------------------------------------

test_that("'make_draws_intercept' works", {
    draws_all <- matrix((1:200)/100, nc = 10)
    ans_obtained <- make_draws_intercept(draws_all)
    ans_expected <- tibble(draw = 1:20,
                           .value = draws_all[,1])
    expect_identical(ans_obtained, ans_expected)
})


## 'make_draws_hyper' ---------------------------------------------------------

test_that("'make_draws_hyper' works", {
    draws_all <- matrix((1:200)/100, nc = 10)
    offset <- 2L
    spec <- RW2()
    ans_obtained <- make_draws_hyper(draws_all = draws_all,
                                     offset = offset,
                                     spec = spec)
    ans_expected <- tibble(draw = c(1:20, 1:20),
                           hyper = c(rep("sd", 20), rep("slope", 20)),
                           .value = c(exp(draws_all[,2]), draws_all[,3]))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_draws_post' ----------------------------------------------------------

test_that("'make_draws_post' works", {
    set.seed(0)
    nevent_df <- expand.grid(age = 0:9, time = 1:5, sex = c("F", "M"),
                             KEEP.OUT.ATTRS = FALSE)
    py_df <- expand.grid(age = 0:9, time = 1:5, sex = c("F", "M"),
                         KEEP.OUT.ATTRS = FALSE)
    nevent_df$nevent <- rpois(n = nrow(nevent_df), lambda = 10)
    py_df$py <- 100
    obj <- smooth_agetime(nevent_df = nevent_df,
                          py_df = py_df,
                          byvar = "sex",
                          spec_age = RW2(),
                          spec_time = TimeFixed())
    ans <- make_draws_post(object = obj,
                           n_draw = 10)
    expect_identical(names(ans),
                     c("rates", "intercept", "age_effect",
                       "time_effect", "age_hyper", "time_hyper"))
    nd <- sapply(ans, function(x) length(unique(x$draw)))
    expect_true(all(nd == nd[1]))
    draw_is_int <- sapply(ans, function(x) is.integer(x$draw))
    expect_true(all(draw_is_int))
})



## 'make_draws_post_one_notime' -----------------------------------------------

test_that("'make_draws_post_one_notime' works", {
    set.seed(0)
    nevent <- matrix(rpois(20, lambda = 1:20),
                     nrow = 20,
                     ncol = 1,
                     dimnames = list(AGE = 0:19, NULL))
    nevent_df <- as.data.frame.table(nevent, stringsAsFactors = FALSE)
    nevent_df$AGE <- factor(nevent_df$AGE)
    py <- matrix(1000,
                 nrow = 20,
                 ncol = 1,
                 dimnames = list(AGE = 0:19, NULL))
    spec_age <- RW2()
    spec_time <- new_BayesRates_spec_timenull()
    fitted <- make_fitted(nevent = nevent,
                          py = py,
                          spec_age = spec_age,
                          spec_time = spec_time)
    ans <- make_draws_post_one_notime(fitted = fitted,
                                      agevar = "AGE",
                                      n_draw = 10)
    expect_identical(names(ans),
                     c("rates", "intercept", "age_effect", "age_hyper"))
    nd <- sapply(ans, function(x) length(unique(x$draw)))
    expect_true(all(nd == nd[1]))
    draw_is_int <- sapply(ans, function(x) is.integer(x$draw))
    expect_true(all(draw_is_int))
})


## 'make_draws_post_one_notime' -----------------------------------------------

test_that("'make_draws_post_one_notime' works", {
    set.seed(0)
    nevent <- matrix(rpois(20, lambda = 1:20),
                     nrow = 20,
                     ncol = 1,
                     dimnames = list(AGE = 0:19, NULL))
    nevent_df <- as.data.frame.table(nevent, stringsAsFactors = FALSE)
    nevent_df$AGE <- factor(nevent_df$AGE)
    py <- matrix(1000,
                 nrow = 20,
                 ncol = 1,
                 dimnames = list(AGE = 0:19, NULL))
    spec_age <- RW2()
    spec_time <- new_BayesRates_spec_timenull()
    fitted <- make_fitted(nevent = nevent,
                          py = py,
                          spec_age = spec_age,
                          spec_time = spec_time)
    ans <- make_draws_post_one_notime(fitted = fitted,
                                      agevar = "AGE",
                                      n_draw = 10)
    expect_identical(names(ans),
                     c("rates", "intercept", "age_effect", "age_hyper"))
    nd <- sapply(ans, function(x) length(unique(x$draw)))
    expect_true(all(nd == nd[1]))
    draw_is_int <- sapply(ans, function(x) is.integer(x$draw))
    expect_true(all(draw_is_int))
})


## 'make_draws_post_one_withtime' ---------------------------------------------

test_that("'make_draws_post_one_withtime' works", {
    set.seed(0)
    nevent <- matrix(rpois(100, lambda = rep(1:20, each = 50)),
                     nrow = 5,
                     ncol = 20,
                     dimnames = list(AGE = 0:4, TIME = 2001:2020))
    nevent_df <- as.data.frame.table(nevent, stringsAsFactors = FALSE)
    nevent_df$AGE <- factor(nevent_df$AGE)
    nevent_df$TIME <- as.integer(nevent_df$TIME)
    py <- matrix(1000,
                 nrow = 5,
                 ncol = 20,
                 dimnames = list(AGE = 0:4, TIME = 2001:2020))
    spec_age <- RW2()
    spec_time <- TimeVarying()
    fitted <- make_fitted(nevent = nevent,
                          py = py,
                          spec_age = spec_age,
                          spec_time = spec_time)
    ans <- make_draws_post_one_withtime(fitted = fitted,
                                        agevar = "AGE",
                                        timevar = "TIME",
                                        n_draw = 10L)
    expect_identical(names(ans),
                     c("rates", "intercept", "age_effect",
                       "time_effect", "age_hyper", "time_hyper"))
    nd <- sapply(ans, function(x) length(unique(x$draw)))
    expect_true(all(nd == nd[1]))
    draw_is_int <- sapply(ans, function(x) is.integer(x$draw))
    expect_true(all(draw_is_int))
})


## 'make_draws_time_effect' ---------------------------------------------------

test_that("'make_draws_time_effect' works - main effect", {
    draws_all <- matrix((1:200)/100, nc = 10)
    offset <- 7L
    labels_time <- c("2000", "2001", "2002", "2003")
    colnames(draws_all) <- c(rep("", 6), paste0("effect.", labels_time))
    spec_time <- TimeFixed()
    X_time <- make_X_time(spec_time, labels_time = labels_time)
    ans_obtained <- make_draws_time_effect(draws_all = draws_all,
                                           offset = offset,
                                           spec_time = spec_time,
                                           agevar = NULL,
                                           timevar = "TIME",
                                           X_age_subspace = NULL,
                                           X_time = X_time)
    ans_expected <- tibble(draw = rep(1:20, 4),
                           TIME = rep(labels_time, each = 20),
                           .value = as.numeric(draws_all[,7:9] %*% t(as.matrix(X_time))))
    expect_identical(ans_obtained, ans_expected)
})

test_that("'make_draws_time_effect' works - interaction", {
    draws_all <- matrix((1:200)/100, nc = 10)
    offset <- 2L
    labels_age <- c("0-4", "5-9", "10-14")
    labels_time <- c("2000", "2001", "2002", "2003")
    colnames(draws_all) <- c(rep("", 6), paste0("effect.", labels_time))
    spec_age <- RW2()
    spec_time <- TimeVarying()
    X_age_subspace <- make_X_age_subspace(spec_age, labels_age = labels_age)
    X_time <- make_X_time(spec_time, labels_time = labels_time)
    ans_obtained <- make_draws_time_effect(draws_all = draws_all,
                                           offset = offset,
                                           spec_time = spec_time,
                                           agevar = "AGE",
                                           timevar = "TIME",
                                           X_age_subspace = X_age_subspace,
                                           X_time = X_time)
    ans_expected <- tibble(draw = rep(1:20, 12),
                           AGE = factor(rep(rep(labels_age, each = 20), times = 4), levels = labels_age),
                           TIME = rep(labels_time, each = 60),
                           .value = as.numeric(matrix(draws_all[,2:10], ncol = 3) %*% t(as.matrix(X_time))))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_fitted' --------------------------------------------------------------

test_that("'make_fitted' works with RW2 age model and TimeFixed time model", {
    set.seed(0)
    nevent <- matrix(rpois(50, lambda = rep(1:10, each = 5)),
                     nrow = 5,
                     ncol = 10,
                     dimnames = list(age = 0:4, time = 2001:2010))
    py <- matrix(100,
                 nrow = 5,
                 ncol = 10,
                 dimnames = list(age = 0:4, time = 2001:2010))
    spec_age <- RW2()
    spec_time <- TimeFixed()
    ans <- make_fitted(nevent = nevent,
                       py = py,
                       spec_age = spec_age,
                       spec_time = spec_time)
    expect_identical(names(ans),
                     c("mean", "prec", "spec_age", "spec_time",
                       "X_age_parfree", "X_age_subspace",
                       "X_time", "convergence"))
    expect_identical(length(unlist(ans$mean)), nrow(ans$prec))
})

test_that("'make_fitted' works with RW2 age model and TimeVarying time model", {
    set.seed(0)
    nevent <- matrix(rpois(50, lambda = rep(1:10, each = 5)),
                     nrow = 5,
                     ncol = 10,
                     dimnames = list(age = 0:4, time = 2001:2010))
    py <- matrix(100,
                 nrow = 5,
                 ncol = 10,
                 dimnames = list(age = 0:4, time = 2001:2010))
    spec_age <- RW2()
    spec_time <- TimeVarying()
    ans <- make_fitted(nevent = nevent,
                       py = py,
                       spec_age = spec_age,
                       spec_time = spec_time)
    expect_identical(names(ans),
                     c("mean", "prec", "spec_age", "spec_time",
                       "X_age_parfree", "X_age_subspace",
                       "X_time", "convergence"))
    expect_identical(length(unlist(ans$mean)), nrow(ans$prec))
})


## 'make_probs' ---------------------------------------------------------------

test_that("'make_probs' works", {
    expect_equal(make_probs(0.95),
                 c(0.025, 0.5, 0.975))
    expect_equal(make_probs(0),
                 c(0.5, 0.5, 0.5))
    expect_equal(make_probs(1L),
                 c(0, 0.5, 1))
    expect_error(make_probs(1.1))
    expect_error(make_probs(-1))
})


## 'make_rw_matrix' -----------------------------------------------------------

test_that("'make_rw_matrix' works", {
    set.seed(0)
    n <- 10
    m <- make_rw_matrix(n)
    x <- rnorm(n - 1)
    y <- as.numeric(m %*% x)
    expect_equal(sum(y), 0)
    expect_equal(diff(y), x)
})


## 'make_rw2_matrix' ----------------------------------------------------------

test_that("'make_rw2_matrix' works", {
    set.seed(0)
    n <- 10
    m <- make_rw2_matrix(n)
    x <- rnorm(n - 2)
    y <- as.numeric(m %*% x)
    expect_equal(sum(y), 0)
    expect_equal(diff(diff(y)), x)
})


## 'make_spline_matrix' -------------------------------------------------------

test_that("'make_spline_matrix' works", {
    set.seed(0)
    n <- 10
    df <- 5
    m <- make_spline_matrix(n = n, df = df)
    expect_equal(dim(m), c(10L, 5L))
    expect_true(all(rowSums(as.matrix(m)) > 0))
})


## 'make_vals_by' -------------------------------------------------------------

test_that("'make_vals_by' works", {
    df <- expand.grid(age = 0:4, sex = c("F", "M"), reg = c("a", "b"), time = 2001:2005,
                      KEEP.OUT.ATTRS = FALSE,
                      stringsAsFactors = FALSE)
    df$nevent <- 2
    df$py <- 10
    byvar <- c("reg", "sex")
    ans_obtained <- make_vals_by(df = df, byvar = byvar)
    ans_expected <- list('a.F' = tibble(reg = "a", sex = "F"),
                         'b.F' = tibble(reg = "b", sex = "F"),
                         'a.M' = tibble(reg = "a", sex = "M"),
                         'b.M' = tibble(reg = "b", sex = "M"))
    expect_identical(ans_obtained, ans_expected)
})


## 'merge_draws_with_vals_by' -------------------------------------------------

test_that("'merge_draws_with_vals_by' works", {
    set.seed(0)
    nevent <- matrix(rpois(100, lambda = rep(1:20, each = 50)),
                     nrow = 5,
                     ncol = 20,
                     dimnames = list(AGE = 0:4, TIME = 2001:2020))
    nevent_df <- as.data.frame.table(nevent, stringsAsFactors = FALSE)
    nevent_df$AGE <- factor(nevent_df$AGE)
    nevent_df$TIME <- as.integer(nevent_df$TIME)
    py <- matrix(1000,
                 nrow = 5,
                 ncol = 20,
                 dimnames = list(AGE = 0:4, TIME = 2001:2020))
    spec_age <- RW2()
    spec_time <- TimeFixed()
    fitted <- make_fitted(nevent = nevent,
                          py = py,
                          spec_age = spec_age,
                          spec_time = spec_time)
    draws <- make_draws_post_one_withtime(fitted = fitted,
                                          agevar = "AGE",
                                          timevar = "TIME",
                                          n_draw = 10)
    draws <- list(draws, draws)
    df <- data.frame(rbind(nevent_df[c("AGE", "TIME")], nevent_df[c("AGE", "TIME")]),
                     sex = rep(c("Female", "Male"), each = 100))
    vals_by <- make_vals_by(df, byvar = "sex")
    ans <- merge_draws_with_vals_by(draws = draws,
                                    vals_by = vals_by)
    expect_identical(names(ans),
                     c("rates",
                       "intercept",
                       "age_effect",
                       "time_effect",
                       "age_hyper",
                       "time_hyper"))
    expect_true(all(sapply(ans, tibble::is_tibble)))
})


## 'reformat_var_age' ---------------------------------------------------------

test_that("'reformat_var_age' works with valid input", {
    expect_identical(reformat_var_age(var_current = c(0, 1),
                                      var_target = 0:1,
                                      agevar = "Age"),
                     0:1)
    expect_identical(reformat_var_age(var_current = c(0, 1),
                                      var_target = c(1, 0, 2),
                                      agevar = "Age"),
                     c(0, 1))
    expect_identical(reformat_var_age(var_current = c(0, 1),
                                      var_target = factor(c(1, 0, 2), levels = 2:0),
                                      agevar = "Age"),
                     factor(c(0, 1), levels = 2:0))
    expect_identical(reformat_var_age(var_current = 0:1,
                                      var_target = "2",
                                      agevar = "Age"),
                     c("0", "1"))
})

test_that("'reformat_var_age' throws correct error with invalid input", {
    expect_error(reformat_var_age(var_current = 0:1,
                                  var_target = NULL,
                                  agevar = "Age"),
                 "'Age' has class \"NULL\"")
})


## 'reformat_var_time' ---------------------------------------------------------

test_that("'reformat_var_time' works with valid input", {
    expect_identical(reformat_var_time(var_current = c(0, 1),
                                       var_target = 0:1,
                                       timevar = "Time"),
                     0:1)
    expect_identical(reformat_var_time(var_current = c(0, 1),
                                       var_target = c(1, 0, 2),
                                       timevar = "Time"),
                     c(0, 1))
})

test_that("'reformat_var_time' throws correct error with invalid input", {
    expect_error(reformat_var_time(var_current = 0:1,
                                   var_target = "a",
                                  timevar = "Time"),
                 "'Time' has class \"character\"")
})


## 'rmvn' ---------------------------------------------------------------------

test_that("'rmvn' gives correct answer with valid inputs", {
    set.seed(0)
    mean <- rnorm(3)
    prec <- matrix(runif(n = 9, max = 10), nr = 3)
    prec <- t(prec) %*% prec
    ans <- rmvn(n = 100000,
                mean = mean,
                prec = prec)
    expect_equal(colMeans(ans), mean, tolerance = 0.01)
    expect_equal(solve(cov(ans)), prec, tolerance = 0.01)
})
