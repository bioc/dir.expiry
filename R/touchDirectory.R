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
#' This function is used to update the last-accessed timestamp for a particular versioned directory.
#' The timestamp is stored in a \code{*_dir.expiry} stub file inside \code{path},
#' and is checked by \code{\link{clearDirectories}} to determine whether the directory is old enough for deletion.
#' We use an explicit timestamp instead of relying on POSIX access times as the latter may not be reliable indicators of genuine access
#' (e.g., during filesystem scans for viruses or to create backups) or may be turned off altogether for performance reasons.
# 
#' The \code{touchDirectory} function should be used in the following manner to ensure thread safety:
#' \enumerate{
#' \item The user should first call \code{\link{lockDirectory}(path)} before calling \code{touchDirectory(path)}.
#' This ensures that another process calling \code{\link{clearDirectories}} does not delete \code{path} while its access time is being updated.
#' \item The caller should then perform the desired operations on \code{path}.
#' This may be a read-only operation if the V lock is a shared lock or a read/write operation for an exclusive lock.
#' \item Once the operation has completed successfully, the caller should call \code{touchDirectory(path)}.
#' This ensures that the most recent timestamp is recorded, especially for long-running operations.
#' Multiple processes may safely call \code{touchDirectory(path)} when the V lock is shared.
#' \item Finally, the user should call \code{\link{unlockDirectory}(path)}.
#' This is typically wrapped in an \code{\link{on.exit}} to ensure that the locks are removed.
#' }
#'
#' For a given \code{path} and \code{version}, this function only modifies the files on its first call.
#' All subsequent calls with the same two arguments, in the same R session, and on the same day will have no effect.
#' This avoids unnecessary filesystem writes and locks when this function is repeatedly called.
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
