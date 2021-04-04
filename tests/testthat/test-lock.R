# Tests the lockDirectory function.
# library(testthat); library(dir.expiry); source("test-lock.R")

test_that("lockDirectory generates the requisite files", {
    base.path <- tempfile(pattern="expired_demo")
    dir.create(base.path)
    version <- package_version("1.11.0")

    handle <- lockDirectory(file.path(base.path, version))
    expect_identical(length(handle), 3L)

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

test_that("unlockDirectory calls the directory clearer", {
    path <- tempfile(pattern="expired_demo")

    dir.create(path)

    # Making one expired directory.
    version <- package_version("1.11.0")
    ver.dir <- file.path(path, version)
    dir.create(ver.dir)
    touchDirectory(ver.dir, date=Sys.Date() - 100)

    # Another versioned directory.
    version <- package_version("1.12.0")
    ver.dir <- file.path(path, version)
    dir.create(ver.dir)
    touchDirectory(ver.dir)

    # Locking and unlocking without calling the directory clearer. 
    lck <- lockDirectory(ver.dir)
    unlockDirectory(lck, clear=FALSE)

    all.files <- list.files(path)
    expect_true("1.12.0" %in% all.files)
    expect_true("1.12.0_dir.expiry" %in% all.files)
    expect_true("1.11.0" %in% all.files)
    expect_true("1.11.0_dir.expiry" %in% all.files)

    # And again, this time with the clearer.
    lck <- lockDirectory(ver.dir)
    unlockDirectory(lck)

    all.files <- list.files(path)
    expect_true("1.12.0" %in% all.files)
    expect_true("1.12.0_dir.expiry" %in% all.files)
    expect_false("1.11.0" %in% all.files)
    expect_false("1.11.0_dir.expiry" %in% all.files)
})

test_that("lockDirectory handles errors and sharedness correctly", {
    tmp <- file.path(tempfile(), "1.0.0")

    loc <- lockDirectory(tmp)
    expect_true(file.exists(paste0(tmp, "-00LOCK")))
    expect_error(lockDirectory(tmp, exclusive=FALSE), "exclusive")
    expect_null(unlockDirectory(loc))

    loc1 <- lockDirectory(tmp, exclusive=FALSE)
    loc2 <- lockDirectory(tmp, exclusive=FALSE)
    expect_error(lockDirectory(tmp, exclusive=TRUE), "shared")
    unlockDirectory(loc1)
    unlockDirectory(loc2)
})
