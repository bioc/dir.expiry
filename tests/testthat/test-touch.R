# This tests the touchDirectories function.
# library(testthat); library(dir.expiry); source("test-touch.R")

test_that("touchDirectories works as expected", {
    path <- tempfile(pattern="expired_demo")
    dir.create(path)
    version <- package_version("1.11.0")

    touchDirectory(file.path(path, version))
    expect_true(file.exists(file.path(path, "1.11.0_dir.expiry")))

    contents <- read.dcf(file.path(path, "1.11.0_dir.expiry"))
    expect_identical(as.character(as.integer(Sys.Date())), unname(contents[,"AccessDate"]))
})

test_that("touchDirectories skips work", {
    path <- tempfile(pattern="expired_demo")

    dir.create(path)
    version <- package_version("1.11.0")
    ver.dir <- file.path(path, version)

    touchDirectory(ver.dir)
    target <- file.path(path, "1.11.0_dir.expiry")
    expect_true(file.exists(target))

    # Doesn't bother to regenerate the file, because we skip this process entirely.
    unlink(target)
    touchDirectory(ver.dir)
    expect_false(file.exists(target))
})
