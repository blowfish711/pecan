#' Matplot generic method
#'
#' @export
matplot <- function(...) {
  UseMethod("matplot")
}

#' Matplot default method
#'
#' @export
matplot.default <- function(...) graphics::matplot(...)

#' Plot multiple spectra on same graph
#'
#' @inheritParams is_spectra
#' @inheritParams graphics::matplot
#' @param ... Additional arguments to `matplot`
#' @export
matplot.spectra <- function(spectra, type = "l", ...) {
  wavelength <- wavelengths(spectra)
  value <- spectra
  matplot(x = wavelength, y = value, type = type, ...)
}
