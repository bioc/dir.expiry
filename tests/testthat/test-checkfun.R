# This tests the makeCheckFunction() function.
# library(testthat); library(dir.expiry); source("test-checkfun.R")

test_that(".check_expiry_function works as expected", {
    expect_true(dir.expiry:::.check_for_expiry("rabbit"))
    expect_false(dir.expiry:::.check_for_expiry("rabbit"))
    expect_false(dir.expiry:::.check_for_expiry("rabbit"))

    expect_true(dir.expiry:::.check_for_expiry("dog"))
    expect_false(dir.expiry:::.check_for_expiry("dog"))
    expect_false(dir.expiry:::.check_for_expiry("dog"))
})

test_that("makeCheckFunction responds to the date", {
    expect_true(dir.expiry:::.check_for_expiry("rabbit2"))

    ENV <- dir.expiry:::expiry.env
    ENV$status$rabbit2$last.checked <- Sys.Date() -100
    expect_true(dir.expiry:::.check_for_expiry("rabbit2"))

    ENV$status$rabbit2$last.checked <- Sys.Date()
    expect_false(dir.expiry:::.check_for_expiry("rabbit2"))
    expect_false(dir.expiry:::.check_for_expiry("rabbit2"))
})
