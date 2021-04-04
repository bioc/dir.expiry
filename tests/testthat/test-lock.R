# Tests the lockDirectory function.
# library(testthat); library(dir.expiry); source("test-lock.R")

test_that("lockDirectory generates the requisite files", {
    base.path <- tempfile(pattern="expired_demo")
    dir.create(base.path)
    version <- package_version("1.11.0")

    handle <- lockDirectory(file.path(base.path, version))
    expect_identical(length(handle), 2L)

    expect_true(file.exists(file.path(base.path, paste0(version, "-00LOCK"))))
    expect_true(file.exists(file.path(base.path, "central-00LOCK")))
    unlockDirectory(handle)
})

test_that("lockDirectory works when the directory isn't yet generated", {
    base.path <- tempfile(pattern="expired_demo")
    version <- "1.0.0"
    handle <- lockDirectory(file.path(base.path, version))

    expect_true(file.exists(base.path))
    expect_true(file.exists(file.path(base.path, paste0(version, "-00LOCK"))))
    expect_true(file.exists(file.path(base.path, "central-00LOCK")))

    unlockDirectory(handle)
})

