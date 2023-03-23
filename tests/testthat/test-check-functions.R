
## 'check_gt_zero' ------------------------------------------------------------

test_that("'check_gt_zero' returns TRUE with valid value", {
    expect_true(check_gt_zero(0.01, nm = "x"))
    expect_true(check_gt_zero(Inf, nm = "x"))
})

test_that("'check_gt_zero' returns TRUE with valid value", {
    expect_error(check_gt_zero(0, nm = "x"),
                 "'x' is less than or equal to 0")
    expect_error(check_gt_zero(-3, nm = "y"),
                 "'y' is less than or equal to 0")
})

    
