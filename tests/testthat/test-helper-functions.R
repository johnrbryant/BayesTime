
## 'combine_draws_effects' ----------------------------------------------------

test_that("'combine_draws_effects' works with valid input", {
    intercept <- rnorm(20)
    age_effect <- matrix(rnorm(80),
                         nr = 20,
                         dimnames = list(draw = 1:20,
                                         Age = c("0-4", "5-9", "10-14", "15-19")))
    time_effect <- matrix(rnorm(100),
                          nr = 20,
                          dimnames = list(draw = 1:20,
                                          Time = 2001:2005))
    ans_obtained <- combine_draws_effects(intercept = intercept,
                                         age_effect = age_effect,
                                         time_effect = time_effect)
    ans_expected <- array(intercept + age_effect,
                          c(20, 4, 5),
                          dimnames = list(draw = 1:20,
                                          Age = c("0-4", "5-9", "10-14", "15-19"),
                                          Time = 2001:2005))
    for (i in 1:5) {
        ans_expected[,,i] <- ans_expected[,,i] + time_effect[,i]
    }
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


## 'make_credible_intervals' --------------------------------------------------

test_that("'make_credible_intervals' works - 'x' has classif vars", {
    set.seed(0)
    x <- expand.grid(age = 0:4,
                     time = 2001:2003,
                     sex = c("F", "M"),
                     draw = 1:20,
                     KEEP.OUT.ATTRS = FALSE)
    x$value <- rnorm(n = nrow(x))
    ans <- make_credible_intervals(x, measurevar = "value", width = 0.9)
    lower <- aggregate(x$value, x[c("age", "time", "sex")], quantile, prob = 0.05)
    lower <- merge(ans, lower)
    expect_equal(lower$.lower, lower$x)
    mid <- aggregate(x$value, x[c("age", "time", "sex")], quantile, prob = 0.5)
    mid <- merge(ans, mid)
    expect_equal(mid$.fitted, mid$x)
    upper <- aggregate(x$value, x[c("age", "time", "sex")], quantile, prob = 0.95)
    upper <- merge(ans, upper)
    expect_equal(upper$.upper, upper$x)
})

test_that("'make_credible_intervals' works - 'x' does not have classif vars", {
    set.seed(0)
    x <- data.frame(draw = 1:100,
                    value = rnorm(100))
    ans_obtained <- make_credible_intervals(x, measurevar = "value", width = 0.9)
    ans_expected <- tibble(.fitted = median(x$value),
                           .lower = as.numeric(quantile(x$value, 0.05)),
                           .upper = as.numeric(quantile(x$value, 0.95)))
    expect_equal(ans_obtained, ans_expected)
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


## 'make_draws_age_effect' ----------------------------------------------------

test_that("'make_draws_age_effect' works", {
    draws_all <- matrix((1:200)/100, nc = 10)
    offset <- 2L
    model <- RW2()
    labels_age <- c("0", "1", "2", "3+")
    X_age <- get_X_age(model,  labels_age = labels_age)
    ans_obtained <- make_draws_age_effect(draws_all = draws_all,
                                          offset = offset,
                                          X_age = X_age,
                                          agevar = "Age")
    ans_expected <- matrix(draws_all[,2:3] %*% t(as.matrix(X_age)),
                           nr = 20,
                           dimnames = list(draw = 1:20, Age = labels_age))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_draws_intercept' -----------------------------------------------------

test_that("'make_draws_intercept' works", {
    draws_all <- matrix((1:200)/100, nc = 10)
    ans_obtained <- make_draws_intercept(draws_all)
    ans_expected <- matrix(draws_all[,1],
                           nr = 20,
                           dimnames = list(draw = 1:20, "(Intercept)" = "(Intercept)"))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_draws_hyper' ---------------------------------------------------------

test_that("'make_draws_hyper' works", {
    draws_all <- matrix((1:200)/100, nc = 10)
    offset <- 2L
    model <- RW2()
    ans_obtained <- make_draws_hyper(draws_all = draws_all,
                                     offset = offset,
                                     model = model)
    ans_expected <- matrix(exp(draws_all[,2]),
                           nr = 20,
                           dimnames = list(draw = 1:20, hyper = "sd"))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_draws_time_effect' ---------------------------------------------------

test_that("'make_draws_time_effect' works", {
    draws_all <- matrix((1:200)/100, nc = 10)
    offset <- 7L
    labels_time <- c("2000", "2001", "2002", "2003")
    colnames(draws_all) <- c(rep("", 6), paste0("effect.", labels_time))
    ans_obtained <- make_draws_time_effect(draws_all = draws_all,
                                           offset = offset,
                                           timevar = "TIME")
    ans_expected <- matrix(draws_all[,7:10],
                           nr = 20,
                           dimnames = list(draw = 1:20, TIME = labels_time))
    expect_identical(ans_obtained, ans_expected)
})


## 'make_fitted' --------------------------------------------------------------

test_that("'make_fitted' works with RW2 age model and AR1 time model", {
    set.seed(0)
    nevent <- matrix(rpois(50, lambda = rep(1:10, each = 5)),
                     nrow = 5,
                     ncol = 10,
                     dimnames = list(age = 0:4, time = 2001:2010))
    py <- matrix(100,
                 nrow = 5,
                 ncol = 10,
                 dimnames = list(age = 0:4, time = 2001:2010))
    model_age <- RW2()
    model_time <- AR1()
    ans <- make_fitted(nevent = nevent,
                       py = py,
                       model_age = model_age,
                       model_time = model_time)
    expect_identical(names(ans), c("mean", "var", "model_age", "model_time", "X_age"))
    expect_identical(length(unlist(ans$mean)), nrow(ans$var))
})

test_that("'make_fitted' works with RW2 age model and LocalTrend time model", {
    set.seed(0)
    nevent <- matrix(rpois(50, lambda = rep(1:10, each = 5)),
                     nrow = 5,
                     ncol = 10,
                     dimnames = list(age = 0:4, time = 2001:2010))
    py <- matrix(100,
                 nrow = 5,
                 ncol = 10,
                 dimnames = list(age = 0:4, time = 2001:2010))
    model_age <- RW2()
    model_time <- LocalTrend()
    ans <- make_fitted(nevent = nevent,
                       py = py,
                       model_age = model_age,
                       model_time = model_time)
    expect_identical(names(ans), c("mean", "var", "model_age", "model_time", "X_age"))
    expect_identical(length(unlist(ans$mean)), nrow(ans$var))
})


## 'make_post_draws' ----------------------------------------------------------

test_that("'make_post_draws' works", {
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
    model_age <- RW2()
    model_time <- AR1()
    fitted <- make_fitted(nevent = nevent,
                          py = py,
                          model_age = model_age,
                          model_time = model_time)
    ans <- make_post_draws(fitted = fitted,
                           n_draw = 10,
                           nevent_df = nevent_df,
                           agevar = "AGE",
                           timevar = "TIME")
    expect_identical(names(ans),
                     c("rates", "intercept", "age_effect",
                       "time_effect", "age_hyper", "time_hyper"))
    nd <- sapply(ans, function(x) length(unique(x$draw)))
    expect_true(all(nd == nd[1]))
    draw_is_int <- sapply(ans, function(x) is.integer(x$draw))
    expect_true(all(draw_is_int))
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


## 'make_rw2_matrix' ----------------------------------------------------------

test_that("'make_rw2_matrix' works", {
    set.seed(0)
    n <- 10
    m <- make_rw2_matrix(n)
    x <- rnorm(n - 2)
    y <- as.numeric(m %*% x)
    expect_equal(sum(y), 0)
    expect_equal(sum(diff(y)), 0)
    expect_equal(diff(diff(y)), x)
})


## 'make_spline_matrix' ----------------------------------------------------------

test_that("'make_spline_matrix' works", {
    set.seed(0)
    n <- 10
    df <- 5
    m <- make_spline_matrix(n = n, df = df)
    expect_equal(dim(m), c(10L, 5L))
    expect_equal(sum(as.matrix(m)[1,]), 0)
    expect_true(all(rowSums(as.matrix(m)[-1,]) > 0))
})


## 'merge_byvar_with_post' ----------------------------------------------------

test_that("'merge_byvar_with_post' works", {
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
    model_age <- RW2()
    model_time <- AR1()
    fitted <- make_fitted(nevent = nevent,
                          py = py,
                          model_age = model_age,
                          model_time = model_time)
    post <- make_post_draws(fitted = fitted,
                            n_draw = 10,
                            nevent_df = nevent_df,
                            agevar = "AGE",
                            timevar = "TIME")
    post <- list(post, post)
    data <- list(data.frame(sex = "Female"),
                 data.frame(sex = "Male"))
    ans <- merge_byvar_with_post(post_draws = post,
                                 data = data,
                                 byvar = "sex")
    expect_identical(names(ans),
                     c("rates",
                       "intercept",
                       "age_effect",
                       "time_effect",
                       "age_hyper",
                       "time_hyper"))
    expect_true(all(sapply(ans, tibble::is_tibble)))
})



## 'reformat_array' -----------------------------------------------------------

test_that("'reformat_array' works with valid input", {
    x <- matrix(1:4, nr = 2, dimnames = list(draw = 1:2, time = 2001:2002))
    ans_obtained <- reformat_array(x)
    ans_expected <- tibble(draw = c(1:2, 1:2),
                           time = as.character(c(2001, 2001, 2002, 2002)),
                           value = 1:4)
    expect_identical(ans_obtained, ans_expected)
})


## 'reformat_age_effect' ------------------------------------------------------

test_that("'reformat_age_effect' works with valid input", {
    x <- matrix(1:4,
                nr = 2,
                dimnames = list(draw = 1:2, age = 0:1))
    df <- data.frame(age = factor(c("0", "1", "2")))
    ans_obtained <- reformat_age_effect(x, df = df, agevar = "age")
    ans_expected <- tibble(draw = rep(1:2, times = 2),
                           age = factor(c("0", "0", "1", "1"), levels = 0:2),
                           value = 1:4)
    expect_identical(ans_obtained, ans_expected)
})


## 'reformat_intercept' -------------------------------------------------------

test_that("'reformat_intercept' works with valid input", {
    x <- matrix(11:14,
                nr = 4,
                dimnames = list(draw = 1:4,  "(Intercept)" = "(Intercept)"))
    ans_obtained <- reformat_intercept(x)
    ans_expected <- tibble(draw = 1:4,
                           value = 11:14)
    expect_identical(ans_obtained, ans_expected)
})


## 'reformat_rates' -----------------------------------------------------------

test_that("'reformat_rates' works with valid input", {
    x <- array(1:8,
               dim = c(2, 2, 2),
               dimnames = list(draw = 1:2, age = 0:1, time = 2001:2002))
    df <- data.frame(age = factor(c("0", "1", "2")),
                     time = c(2002, 2001, 2000))
    ans_obtained <- reformat_rates(x, df = df, agevar = "age", timevar = "time")
    ans_expected <- tibble(draw = rep(1:2, times = 4),
                           age = factor(rep(c("0", "0", "1", "1"), times = 2), levels = 0:2),
                           time = rep(c(2001, 2002), each = 4),
                           value = 1:8)
    expect_identical(ans_obtained, ans_expected)
})


## 'reformat_time_effect' -----------------------------------------------------

test_that("'reformat_time_effect' works with valid input", {
    x <- matrix(1:4,
                nr = 2,
                dimnames = list(draw = 1:2, time = 0:1))
    df <- data.frame(time = c(1, 0))
    ans_obtained <- reformat_time_effect(x, df = df, timevar = "time")
    ans_expected <- tibble(draw = rep(1:2, times = 2),
                           time = c(0, 0, 1, 1),
                           value = 1:4)
    expect_identical(ans_obtained, ans_expected)
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


    
    


