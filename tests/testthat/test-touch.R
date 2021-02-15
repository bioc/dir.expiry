# This tests the touchDirectories function.
# library(testthat); library(dir.expiry); source("test-touch.R")

test_that("touchDirectories works as expected", {
    path <- tempfile(pattern="expired_demo")
    dir.create(path)
    version <- package_version("1.11.0")

    touchDirectory(path, version)
    expect_true(file.exists(file.path(path, "1.11.0_dir.expiry-info")))

    contents <- read.dcf(file.path(path, "1.11.0_dir.expiry-info"))
    expect_identical(as.character(as.integer(Sys.Date())), unname(contents[,"AccessDate"]))
})

test_that("touchDirectories calls the directory clearer", {
    path <- tempfile(pattern="expired_demo")

    dir.create(path)
    version <- package_version("1.11.0")
    touchDirectory(path, version, date=Sys.Date() - 100, clear=FALSE)
    dir.create(file.path(path, version))
    expect_true(length(list.files(path)) == 3L)
    expect_true("1.11.0" %in% list.files(path))

    version <- package_version("1.12.0")
    touchDirectory(path, version)
    dir.create(file.path(path, version))
    expect_true(length(list.files(path)) == 3L)
    expect_true("1.12.0" %in% list.files(path))
    expect_false("1.11.0" %in% list.files(path))
})
