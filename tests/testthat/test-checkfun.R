# This tests the makeCheckFunction() function.
# library(testthat); library(dir.expiry); source("test-checkfun.R")

test_that(".was_checked_today works as expected", {
    env <- new.env()
    env$status <- list()

    expect_false(dir.expiry:::.was_checked_today("rabbit", env))
    expect_true(dir.expiry:::.was_checked_today("rabbit", env))
    expect_true(dir.expiry:::.was_checked_today("rabbit", env))

    expect_false(dir.expiry:::.was_checked_today("dog", env))
    expect_true(dir.expiry:::.was_checked_today("dog", env))
    expect_true(dir.expiry:::.was_checked_today("dog", env))
})

test_that(".was_checked_today responds to the date", {
    env <- new.env()
    env$status <- list()
    expect_false(dir.expiry:::.was_checked_today("rabbit2", env))

    env$status$rabbit2$last.checked <- Sys.Date() -100
    expect_false(dir.expiry:::.was_checked_today("rabbit2", env))

    env$status$rabbit2$last.checked <- Sys.Date()
    expect_true(dir.expiry:::.was_checked_today("rabbit2", env))
    expect_true(dir.expiry:::.was_checked_today("rabbit2", env))
})
