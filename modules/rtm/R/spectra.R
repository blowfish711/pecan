#' Spectra S3 class
#'
#' @param spectra Vector (`length = length(wavelengths)`) or matrix (`ncol = length(wavelengths)`)
#' @param wavelengths Wavelengths of spectra. 
#' @param spectra_types Type of spectra, one of "R", "T", "PA", or "CRR" (see [valid_spectra_types]).
#' @export
spectra <- function(spectra, wavelengths = 400:2500, spectra_types = "R") {
  if (!is.matrix(spectra)) {
    spectra <- as.matrix(spectra)
  }
  rownames(spectra) <- NULL
  nwl <- length(wavelengths)
  nr <- nrow(spectra)
  if (length(wavelengths) != nrow(spectra)) {
    err <- sprintf("Number of wavelengths (%d) does not match rows in matrix (%d)", nwl, nr)
    stop(err)
  }
  if (!(length(type) == 1 || length(type) == ncol(spectra))) {
    err <- sprintf("Type vector must be length 1 or `ncol(spectra)` (%d). Input is length %d",
                   ncol(spectra), length(type))
    stop(err)
  }
  structure(
    spectra,
    wavelengths = wavelengths,
    class = c("spectra", "matrix"),
    spectra_types = type
  )
}

#' @describeIn spectra Test if object is class `spectra`
#' @export
is_spectra <- function(spectra) inherits(spectra, "spectra")

#' Retrieve wavelengths from spectra object
#'
#' @param spectra Object of class `spectra`
#' @export
wavelengths <- function(spectra) attr(spectra, "wavelengths")

#' Retrieve spectra type of spectra object
#'
#' @inheritParams wavelengths
#' @export
spectra_types <- function(spectra) attr(spectra, "spectra_types")

#' Valid types of spectra, for use in [spectra], [invert_fieldspec], and elsewhere
#'
#' Spectra type must be one of the following:
#'  - "R" -- Reflectance (default)
#'  - "T" -- Transmittance
#'  - "PA" -- Pseudo-absorbance, defined as `log10(1 / reflectance)`
#'  - "CRR" -- Continuum-removed reflectance. See 
#'  [prospectr::continuumRemoval]. Requires the `prospectr` package.
#'
#' @export
valid_spectra_types <- c("R", "T", "PA", "CRR")
