#' Touch a versioned directory
#'
#' Touch a versioned directory to indicate that it has been successfully accessed in the recent past.
#'
#' @param path String containing the path to a versioned directory.
#' The \code{dirname} should be the package cache while the \code{basename} should be a version number.
#' @param date A \link{Date} object containing the current date.
#' Only provided for testing.
#' @param force Logical scalar indicating whether to forcibly update the access date for \code{path}.
#'
#' @details
#' This function should be called \emph{after} any successful access to the contents of a versioned directory,
#' to indicate that said directory is still in use by expiry-aware processes.
#' A stub file is updated with the last access time to allow \code{\link{clearDirectories}} to accurately check for staleness.
#'
#' For a given \code{path} and \code{version}, this function only modifies the files on its first call.
#' All subsequent calls with the same two arguments, in the same R session and on the same day will have no effect.
#' This avoids unnecessary touching of the file system during routine use.
#'
#' The caller should lock the target directory with \code{\link{lockDirectory}} before calling this function.
#' This ensures that another process calling \code{\link{clearDirectories}} does not delete this directory while its access time is being updated.
#' If the target directory is locked, any writes to the stub file itself are thread-safe, even for shared locks.
#'
#' By default, this function will remember the values of \code{path} that were passed in previous calls,
#' and will avoid re-updating those same \code{path}s with the same date when called on the same day.
#' This avoids unnecessary file system writes and locks when this function is repeatedly called.
#' Advanced users can force an update by setting \code{force=TRUE}.
#'
#' @return 
#' The \code{<version>_dir.expiry} stub file within \code{path} is updated/created with the current date.
#' A \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun
#' @examples
#' # Creating the package cache.
#' cache.dir <- tempfile(pattern="expired_demo")
#' dir.create(cache.dir)
#'
#' # Creating the versioned subdirectory.
#' version <- package_version("1.11.0")
#' version.dir <- file.path(cache.dir, version)
#' lck <- lockDirectory(version.dir)
#' dir.create(version.dir)
#' 
#' # Setting the last access time.
#' touchDirectory(version.dir)
#' list.files(cache.dir)
#' readLines(file.path(cache.dir, "1.11.0_dir.expiry"))
#'
#' # Making sure we unlock it afterwards.
#' unlockDirectory(lck)
#' 
#' @seealso
#' \code{\link{lockDirectory}}, which should always be called before this function.
#'
#' @export
#' @importFrom utils packageVersion
#' @importFrom filelock lock unlock
touchDirectory <- function(path, date=Sys.Date(), force=FALSE) {
    out <- paste0(.unslash(path), expiry.suffix)

    # Skipping if we already touched it some time today. Note we have to 
    # check for the existence of the file just in case it got deleted by
    # clearDirectories (e.g., with limit=-Inf) in the meantime.
    if (.was_checked_today(path, touched.env) && !force && file.exists(out)) {
        return(invisible(NULL))
    }

    # Creating a lockfile for the actual expiry file, so that multiple
    # processes can touch it at the same time (e.g., if those processes were
    # otherwise read-only on the directory contents and thus held shared locks
    # via lockDirectory).
    lckfile <- paste0(out, lock.suffix)
    lckhandle <- lock(lckfile)
    on.exit(unlock(lckhandle))

    write.dcf(file=out, cbind(ExpiryVersion=as.character(packageVersion("dir.expiry")), AccessDate=as.integer(date)))

    invisible(NULL)
}

expiry.suffix <- "_dir.expiry"
touched.env <- new.env()
touched.env$status <- list()

.was_checked_today <- function(vpath, env) {
    info <- env$status[[vpath]]

    if (is.null(info)) {
        info <- list(last.checked=Sys.Date())
        env$status[[vpath]] <- info
        FALSE 
    } else {
        if (Sys.Date() == info$last.checked) {
            TRUE 
        } else {
            info$last.checked <- Sys.Date()
            env$status[[vpath]] <- info
            FALSE
        }
    }
}

.flush_cache <- function(env) {
    env$status <- list()
    invisible(NULL)
}
