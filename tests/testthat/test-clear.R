# This tests the clearDirectories functionality.
# library(testthat); library(dir.expiry); source("test-clear.R")

test_that("clearDirectories works as expected", {
    path <- tempfile(pattern="expired_demo")
                                                                      
    dir.create(path)
    version <- package_version("1.11.0")
    touchDirectory(path, version, date=Sys.Date() - 100, clear=FALSE)
    dir.create(file.path(path, version))

    version <- package_version("1.12.0")
    touchDirectory(path, version, clear=FALSE)
    dir.create(file.path(path, version))
    expect_identical(sort(list.files(path)), c("1.11.0", "1.11.0_dir.expiry-info", "1.12.0", "1.12.0_dir.expiry-info", "dir.expiry-00LOCK"))

    # Only deletes the expired directories.
    clearDirectories(path)
    expect_identical(sort(list.files(path)), c("1.12.0", "1.12.0_dir.expiry-info", "dir.expiry-00LOCK"))
})

test_that("clearDirectories responds to the environment variables", {
    path <- tempfile(pattern="expired_demo")
                                                                      
    dir.create(path)
    version <- package_version("1.11.0")
    touchDirectory(path, version, date=Sys.Date() - 100, clear=FALSE)
    dir.create(file.path(path, version))

    expected <- c("1.11.0", "1.11.0_dir.expiry-info", "dir.expiry-00LOCK")
    expect_identical(sort(list.files(path)), expected)

    # Doesn't delete anything if we pass in the right arguments.
    clearDirectories(path, limit=100)
    expect_identical(sort(list.files(path)), expected)

    old <- Sys.getenv("BIOC_DIR_EXPIRY_LIMIT", NA)
    Sys.setenv(BIOC_DIR_EXPIRY_LIMIT=100)
    clearDirectories(path)
    expect_identical(sort(list.files(path)), expected)

    if (is.na(old)) {
        Sys.unsetenv("BIOC_DIR_EXPIRY_LIMIT")
    } else {
        Sys.setenv(BIOC_DIR_EXPIRY_LIMIT=NA)
    }

    # Positive control.
    clearDirectories(path)
    expect_identical(list.files(path), "dir.expiry-00LOCK")
})
