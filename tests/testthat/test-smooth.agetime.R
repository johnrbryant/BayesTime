

## 'smooth.agetime' -----------------------------------------------------------

test_that("'smooth.agetime' works with valid data - no 'by' variables", {
    set.seed(0)
    nevent_df <- expand.grid(age = 0:9, time = 2011:2020,
                             KEEP.OUT.ATTRS = FALSE)
    nevent_df$nevent <- rpois(n = nrow(nevent_df), lambda = outer(11:20, 5:14))
    py_df <- expand.grid(age = 0:9, time = 2011:2020,
                         KEEP.OUT.ATTRS = FALSE)
    py_df$py <- 100
    ans <- smooth.agetime(nevent_df = nevent_df,
                          py_df = py_df,
                          n_draw = 10)
    expect_s3_class(ans, "BayesRates_results")
})

