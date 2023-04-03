
## 'smooth_agetime_inner' -----------------------------------------------------

test_that("'smooth_agetime_inner' works with RW2 age model and AR1 time model", {
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
    ans <- smooth_agetime_inner(nevent = nevent,
                                py = py,
                                model_age = model_age,
                                model_time = model_time)
    expect_identical(names(ans), c("mean", "var"))
    expect_identical(length(unlist(ans$mean)), nrow(ans$var))
})

test_that("'smooth_agetime_inner' works with RW2 age model and LocalTrend time model", {
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
    ans <- smooth_agetime_inner(nevent = nevent,
                                py = py,
                                model_age = model_age,
                                model_time = model_time)
    expect_identical(names(ans), c("mean", "var"))
    expect_identical(length(unlist(ans$mean)), nrow(ans$var))
})

