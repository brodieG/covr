#' Get/set the PATH variable.
#'
#' @param path character vector of paths
#' @return \code{set_path} invisibly returns the old path.
#' @name path
#' @family path
#' @seealso \code{\link{with_path}} to temporarily set the path for a block
#'   of code
NULL

#' @rdname path
get_path <- function() {
  strsplit(Sys.getenv("PATH"), .Platform$path.sep)[[1]]
}

#' @rdname path
set_path <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)

  old <- get_path()
  path <- paste(path, collapse = .Platform$path.sep)
  Sys.setenv(PATH = path)
  invisible(old)
}

#' @rdname path
#' @param after for \code{add_path}, the place on the PATH where the new paths
#'   should be added
add_path <- function(path, after = Inf) {
  set_path(append(get_path(), path, after))
}


#' Test if an object is on the path.
#'
#' @param ... Strings indicating the executables to check for on the path.
#' @family path
#' @keywords internal
on_path <- function(...) {
  commands <- c(...)
  stopifnot(is.character(commands))
  unname(Sys.which(commands) != "")
}
