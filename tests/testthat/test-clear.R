# This tests the clearDirectories functionality.
# library(testthat); library(dir.expiry); source("test-clear.R")

test_that("clearDirectories works as expected", {
    path <- tempfile(pattern="expired_demo")
    dir.create(path)

    # Making an old directory.
    version <- package_version("1.11.0")
    ver.dir <- file.path(path, version)
    dir.create(ver.dir)
    touchDirectory(ver.dir, date=Sys.Date() - 100, clear=FALSE)

    # Making a more recent one without any clearing.
    version <- package_version("1.12.0")
    ver.dir <- file.path(path, version)
    dir.create(ver.dir)
    touchDirectory(ver.dir, clear=FALSE)

    earlier <- c("1.11.0", "1.11.0_dir.expiry")
    later <- c("1.12.0", "1.12.0_dir.expiry")
    expect_true(all(c(earlier, later) %in% list.files(path)))

    # Only deletes the expired directories.
    clearDirectories(path)

    expect_false(any(c(earlier, "1.11.0-00LOCK") %in% list.files(path)))
    expect_true(all(c(later, "1.12.0-00LOCK") %in% list.files(path))) 
    expect_true("central-00LOCK" %in% list.files(path))
})

test_that("clearDirectories responds to the environment variables", {
    path <- tempfile(pattern="expired_demo")
                                                                      
    dir.create(path)
    version <- package_version("1.11.0")
    ver.dir <- file.path(path, version)
    dir.create(ver.dir)
    touchDirectory(ver.dir, date=Sys.Date() - 100, clear=FALSE)

    expected <- c("1.11.0", "1.11.0_dir.expiry") 
    expect_true(all(expected %in% list.files(path)))

    # Doesn't delete anything if we pass in the right arguments.
    clearDirectories(path, limit=100)
    expect_true(all(expected %in% list.files(path)))

    old <- Sys.getenv("BIOC_DIR_EXPIRY_LIMIT", NA)
    Sys.setenv(BIOC_DIR_EXPIRY_LIMIT=100)
    clearDirectories(path)
    expect_true(all(expected %in% list.files(path)))

    if (is.na(old)) {
        Sys.unsetenv("BIOC_DIR_EXPIRY_LIMIT")
    } else {
        Sys.setenv(BIOC_DIR_EXPIRY_LIMIT=NA)
    }

    # Positive control.
    clearDirectories(path)
    expect_false(any(expected %in% list.files(path)))
})

test_that("clearDirectories doesn't delete the reference or newer versions", {
    path <- tempfile(pattern="expired_demo")
                                                                      
    dir.create(path)
    version <- package_version("1.11.0")
    ver.dir <- file.path(path, version)
    dir.create(ver.dir)
    touchDirectory(ver.dir, date=Sys.Date() - 100, clear=FALSE)

    version <- package_version("1.12.0")
    ver.dir <- file.path(path, version)
    dir.create(ver.dir)
    touchDirectory(ver.dir, date=Sys.Date() - 100, clear=FALSE)

    earlier <- c("1.11.0", "1.11.0_dir.expiry")
    later <- c("1.12.0", "1.12.0_dir.expiry")
    expected <- c(earlier, later)
    expect_true(all(expected %in% list.files(path)))

    # Doesn't delete the expired directories.
    clearDirectories(path, reference=package_version("1.11.0"))
    expect_true(all(expected %in% list.files(path)))

    # Keeps the reference.
    clearDirectories(path, reference=package_version("1.12.0"))
    expect_false(any(c(earlier, "1.11.0-00LOCK") %in% list.files(path)))
    expect_true(all(later %in% list.files(path))) # doesn't make the lock for the reference
})
