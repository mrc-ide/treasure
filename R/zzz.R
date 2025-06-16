.onLoad <- function(libname, pkgname) {
  op <- options()
  op.treasure <- list(
    treasure.target_year = 2024,
    treasure.region = "Sub-Saharan Africa"
  )
  toset <- !(names(op.treasure) %in% names(op))
  if (any(toset)) options(op.treasure[toset])
  invisible()
}
