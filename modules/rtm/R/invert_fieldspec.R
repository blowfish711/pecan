#' Quickly invert a field spectrum
#'
#' Automatically defines an RTM model based on the observed wavelengths and 
#' spectra type.
#'
#' Spectra type must be one of the following:
#'  - "R" -- Reflectance (default)
#'  - "T" -- Transmittance
#'  - "RT" -- Reflectance and transmittance, in separate columns. This requires 
#'  that the observation.
#'  - "PA" -- Pseudo-absorbance, defined as `log10(1 / reflectance)`
#'  - "CRR" -- Continuum-removed reflectance. See 
#'  [prospectr::continuumRemoval]. Requires the `prospectr` package.
#'
#' @param spectra Observed spectra to invert. Must be class [spectra].
#' @param type Type of observation, one of "R", "T", "PA", or "CRR" (see Details)
#' @param prospect_version Version of PROSPECT to use. Default is "5B".
#' @param ...  Additional arguments to [invert_bt]
#' @inheritParams resample
#' @inheritParams invert_bt
#' @export
invert_fieldspec <- function(spectra, type = "R", method = "fmm",
                             prospect_version = 5,
                             prior = prospect_bt_prior(prospect_version),
                             ...) {
  used_types <- c("R", "T", "RT", "PA", "CRR")
  if (!type %in% used_types) {
    stop(
      "Invalid type '", type, "'. ",
      "Must be one of the following: ",
      paste(shQuote(used_types), collapse = ", ")
    )
  }

  if (!is_spectra(spectra)) {
    stop(
      "`spectra` argument must be of class spectra. ",
      "See ?PEcAnRTM::spectra"
    )
  }

  if (type == "RT" && ncol(spectra) %% 2 != 0) {
    stop(
      "For type `RT`, spectra must have even number of columns, ",
      "with reflectance and transmittance in alternating columns. ",
      "Input has ", ncol(spectra), " columns."
    )
  }

  rtm <- switch(
    type,
    `R` = function(param) prospect(param, prospect_version)[, 1],
    `T` = function(param) prospect(param, prospect_version)[, 2],
    `RT` = function(param) prospect(param, prospect_version),
    `PA` = function(param) log10(1 / prospect(param, prospect_version)[, 1]),
    `CRR` = function(param) {
      prospectr::continuumRemoval(prospect(param, prospect_version)[, 1])
    }
  )

  waves <- wavelengths(spectra)

  model <- function(param) {
    spec <- rtm(param)
    resample(spec, waves, method = method)
  }

  invert_bt(
    observed = spectra,
    model = model,
    prior = prior,
    ...
  )

}
