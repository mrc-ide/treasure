#' Check vector argument lengths
#'
#' Ensure that all supplied arguments are either scalars or vectors of the same length.
#'
#' @param ... Numeric vectors to check.
#'
#' @return Invisible `NULL`. An error is thrown if lengths are incompatible.
#' @keywords internal
check_lengths <- function(...) {
  args <- list(...)
  lens <- vapply(args, length, integer(1))
  n <- max(lens)
  if (any(lens != 1 & lens != n)) {
    arg_names <- vapply(substitute(list(...))[-1], deparse1, character(1))
    stop(
      sprintf(
        "Arguments %s must have length 1 or be the same length",
        paste(arg_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}
