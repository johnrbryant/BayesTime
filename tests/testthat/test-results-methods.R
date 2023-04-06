

## 'augment' ------------------------------------------------------------------

test_that("'augment' works", {
    set.seed(0)
    nevent_df <- expand.grid(age = 0:9, time = 2011:2020,
                             KEEP.OUT.ATTRS = FALSE)
    nevent_df$nevent <- rpois(n = nrow(nevent_df), lambda = outer(11:20, 5:14))
    py_df <- expand.grid(age = 0:9, time = 2011:2020,
                         KEEP.OUT.ATTRS = FALSE)
    py_df$py <- 100
    results <- smooth.agetime(nevent_df = nevent_df,
                              py_df = py_df,
                              n_draw = 10)
    ans <- augment(results)
    expect_true(all(c(".fitted", ".lower", ".upper", ".observed") %in% names(ans)))
})

