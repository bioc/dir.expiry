#' Touch a versioned directory
#'
#' Touch a versioned directory to indicate that it has been successfully accessed in the recent past.
#'
#' @param path String containing the path to the base directory, i.e., the directory \emph{containing} all versioned subdirectories for a particular application.
#' @param version A \link{package_version} specifying the version to touch.
#' This should follow the Bioconductor versioning format.
#' @param clear Logical scalar indicating whether to remove expired versions.
#' @param date A \link{Date} object containing the current date.
#' Only provided for testing.
#' @param ... Further arguments to pass to \code{\link{clearDirectories}}.
#'
#' @details
#' For a given \code{path} and \code{version}, this function only modifies the files on its first call.
#' All subsequent calls with the same two arguments, in the same R session and on the same day will have no effect.
#' This avoids unnecessary touching of the file system during routine use.
#'
#' Writes to the output \code{*_dir.expiry} file are thread-safe.
#' However, the caller should lock the target directory with \code{\link{lockDirectory}} before calling this function.
#' This ensures that another process calling \code{\link{clearDirectories}} does not delete this directory while its access time is being updated.
#' 
#' @return 
#' The \code{<version>_dir.expiry} file within \code{path} is updated/created with the current date.
#' If \code{clear=TRUE}, expired directories are removed by \code{\link{clearDirectories}}.
#' A \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun
#' @examples
#' # Creating the base directory.
#' base.path <- tempfile(pattern="expired_demo")
#' dir.create(base.path)
#'
#' # Creating the versioned subdirectory.
#' version <- package_version("1.11.0")
#' dir.create(file.path(base.path, version))
#'
#' # Setting the last access time.
#' touchDirectory(base.path, version)
#' list.files(base.path)
#' readLines(file.path(base.path, "1.11.0_dir.expiry"))
#' 
#' @seealso
#' \code{\link{clearDirectories}}, to remove expired directories at the same time.
#'
#' @export
#' @importFrom utils packageVersion
#' @importFrom filelock lock unlock
touchDirectory <- function(path, version, clear=TRUE, date=Sys.Date(), ...) {
    dir <- file.path(path, as.character(version))

    if (.check_for_expiry(dir)) {
        out <- paste0(dir, expiry.suffix)

        lckfile <- paste0(out, lock.suffix)
        lckhandle <- lock(lckfile)
        on.exit(unlock(lckhandle))

        write.dcf(file=out, cbind(ExpiryVersion=as.character(packageVersion("dir.expiry")), AccessDate=as.integer(date)))

        if (clear) {
            clearDirectories(path, reference=version, ...)
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
