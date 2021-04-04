#' Touch a versioned directory
#'
#' Touch a versioned directory to indicate that it has been successfully accessed in the recent past.
#'
#' @param path String containing the path to a versioned directory.
#' The \code{dirname} should be the package cache while the \code{basename} should be a version number.
#' @param clear Logical scalar indicating whether to remove expired versions.
#' @param date A \link{Date} object containing the current date.
#' Only provided for testing.
#' @param ... Further arguments to pass to \code{\link{clearDirectories}}.
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
#' @return 
#' The \code{<version>_dir.expiry} stub file within \code{path} is updated/created with the current date.
#' If \code{clear=TRUE}, expired directories are removed by \code{\link{clearDirectories}}.
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
#' dir.create(file.path(cache.dir, version))
#'
#' # Setting the last access time.
#' touchDirectory(file.path(cache.dir, version))
#' list.files(cache.dir)
#' readLines(file.path(cache.dir, "1.11.0_dir.expiry"))
#' 
#' @seealso
#' \code{\link{clearDirectories}}, to remove expired directories at the same time.
#'
#' @export
#' @importFrom utils packageVersion
#' @importFrom filelock lock unlock
touchDirectory <- function(path, clear=TRUE, date=Sys.Date(), ...) {
    if (.check_for_expiry(path)) {
        out <- paste0(.unslash(path), expiry.suffix)

        lckfile <- paste0(out, lock.suffix)
        lckhandle <- lock(lckfile)
        on.exit(unlock(lckhandle))

        write.dcf(file=out, cbind(ExpiryVersion=as.character(packageVersion("dir.expiry")), AccessDate=as.integer(date)))

        if (clear) {
            clearDirectories(dirname(path), reference=basename(path), ...)
        }
    }

    invisible(NULL)
}

expiry.suffix <- "_dir.expiry"
expiry.env <- new.env()
expiry.env$status <- list()

.check_for_expiry <- function(vpath) {
    info <- expiry.env$status[[vpath]]

    if (is.null(info)) {
        info <- list(last.checked=Sys.Date())
        expiry.env$status[[vpath]] <- info
        TRUE
    } else {
        if (Sys.Date() == info$last.checked) {
            FALSE
        } else {
            info$last.checked <- Sys.Date()
            expiry.env$status[[vpath]] <- info
            TRUE
        }
    }
}

.flush_cache <- function() {
    expiry.env$status <- list()
    invisible(NULL)
}
