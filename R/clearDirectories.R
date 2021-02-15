#' Clear expired directories
#'
#' Remove versioned directories that have passed on expiration limit.
#'
#' @inheritParams touchDirectory
#' @param reference A \link{package_version} specifying the most recently accessed version.
#' @param limit Integer scalar specifying the maximum number of days to have passed before a versioned directory expires.
#' 
#' @return Expired directories are deleted and \code{NULL} is invisibly returned.
#'
#' @author Aaron Lun
#'
#' @details
#' This function checks the last access date in the \code{*_dir.expiry} files in \code{path}.
#' If the last access date is too old, the corresponding subdirectory in \code{path} is treated as expired and is deleted.
#' The age threshold depends on \code{limit}, which defaults to the value of the environment variable \code{BIOC_DIR_EXPIRY_LIMIT}.
#' If this is not specified, it is set to 30 days.
#'
#' If \code{reference} is specified, directories with version numbers greater than (or equal to) \code{reference} are deleted,
#' even if their last access date was older than the specified \code{limit}.
#' This aims to favor the retention of newer versions, which is generally a sensible outcome when the aim is to stay up-to-date.
#' 
#' This function will acquire an exclusive lock on each directory before attempting to delete it.
#' Applications can achieve thread safety by calling \code{\link{lockDirectory}} prior to any operations on the versioned directory.
#' This ensures that \code{clearDirectories} will not delete a directory in use by another process, especially if the latter might update the last access time.
#'
#' @examples
#' # Creating the base directory.
#' base.path <- tempfile(pattern="expired_demo")
#' dir.create(base.path)
#'
#' # Creating an older versioned directory.
#' version <- package_version("1.11.0")
#' touchDirectory(base.path, version, date=Sys.Date() - 100, clear=FALSE)
#' dir.create(file.path(base.path, version))
#' list.files(base.path)
#'
#' # Clearing them out.
#' clearDirectories(base.path)
#' list.files(base.path)
#' 
#' @seealso
#' \code{\link{touchDirectory}}, which calls this function automatically when \code{clear=TRUE}.
#' @export
clearDirectories <- function(path, reference=NULL, limit=NULL) {
    if (is.null(limit)) {
        limit <- Sys.getenv("BIOC_DIR_EXPIRY_LIMIT", "30")
        limit <- as.integer(limit)
    } 

    pattern <- paste0(expiry.suffix, "$")
    all.files <- list.files(path, pattern=pattern)
    if (!is.null(reference)) {
        all.files <- setdiff(all.files, as.character(reference))
    }

    current <- as.integer(Sys.Date())
    for (x in all.files) {
        version <- sub(pattern, "", x)
        .check_other_directory(path, version=version, name=x, date=current, reference=reference, limit=limit)
    }

    invisible(NULL)
}

.check_other_directory <- function(path, version, name, date, reference, limit) {
    lck <- lockDirectory(path, version)
    on.exit(unlockDirectory(lck))

    acc.path <- file.path(path, name)
    last.used <- as.integer(read.dcf(acc.path)[,"AccessDate"])
    diff <- date - last.used

    if (diff > limit && (is.null(reference) || reference > package_version(version))) {
        unlink(acc.path, force=TRUE)
        unlink(paste0(acc.path, lock.suffix), force=TRUE)
        unlink(file.path(path, version), recursive=TRUE, force=TRUE)
    }
}
