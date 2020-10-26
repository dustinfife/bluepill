context("miscellaneous functions")


test_that("estimate_sd works", {
    expect_true(estimate_sd(10, 5, 15, 1)==5)
    expect_true(estimate_sd(10, 5, 15, 2)==2.5)
})


