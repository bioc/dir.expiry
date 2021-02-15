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
#' This function checks the last access date in the \code{*_access} files in \code{path}.
#' If the last access date is too old, the corresponding subdirectory in \code{path} is treated as expired and is deleted.
#' The age threshold depends on \code{limit}, which defaults to the value of the environment variable \code{BIOC_DIR_EXPIRY_LIMIT}.
#' If this is not specified, it is set to 30 days.
#'
#' If \code{reference} is specified, versioned directories with version numbers greater than \code{reference} are not deleted,
#' even if their last access date was older than the specified \code{limit}.
#' This aims to favor the retention of newer versions, which is generally a sensible outcome when the aim is to stay up-to-date.
#' 
#' Developers should not call this function directly but instead use \code{\link{touchDirectory}}.
#' The latter handles locking and applies checks to avoid unnecessary contact with the file system.
#'
#' @examples
#' # Creating the base directory.
#' base.path <- tempfile(pattern="expired_demo")
#' dir.create(base.path)
#'
#' # Creating an older versioned directory.
#' version <- package_version("1.11.0")
#' touchDirectory(path, version, date=Sys.Date() - 100, clear=FALSE)
#' dir.create(file.path(path, version))
#' list.files(path)
#'
#' # Clearing them out.
#' clearDirectories(path)
#' list.files(path)
#' 
#' @seealso
#' \code{\link{touchDirectory}}, which calls this function automatically when \code{clear=TRUE}.
#' @export
clearDirectories <- function(path, reference=NULL, limit=NULL) {
    current <- as.integer(Sys.Date())

    if (is.null(limit)) {
        limit <- Sys.getenv("BIOC_DIR_EXPIRY_LIMIT", "30")
        limit <- as.integer(limit)
    } 

    pattern <- paste0(suffix, "$")
    all.files <- list.files(path, pattern=pattern)
    for (x in all.files) {
        acc.path <- file.path(path, x)
        last.used <- as.integer(read.dcf(acc.path)[,"AccessDate"])
        diff <- current - last.used

        dir <- sub(pattern, "", x)
        if (diff > limit && (is.null(reference) || reference > package_version(dir))) {
            unlink(acc.path, force=TRUE)
            unlink(file.path(path, dir), recursive=TRUE, force=TRUE)
        }
    }

    invisible(NULL)
}
