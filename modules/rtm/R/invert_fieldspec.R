#' Quickly invert a field spectrum
#'
#' Automatically defines an RTM model based on the observed wavelengths and 
#' spectra type.
#'
#' @param spectra Observed spectra to invert. Must be class [spectra].
#' @param prospect_version Version of PROSPECT to use. Default is "5B".
#' @param test Logical. If `TRUE`, test that model and observations match 
#' before starting sampling.
#' @param ...  Additional arguments to [invert_bt]
#' @inheritParams spectra
#' @inheritParams resample
#' @inheritParams invert_bt
#' @export
invert_fieldspec <- function(spectra, type = spectra_types(spectra), method = "fmm",
                             prospect_version = 5,
                             prior = prospect_bt_prior(prospect_version),
                             test = TRUE,
                             ...) {
  if (!type %in% valid_spectra_types) {
    stop(
      "Invalid type '", type, "'. ",
      "Must be one of the following: ",
      paste(valid_spectra_types, collapse = ", ")
    )
  }

  if (length(unique(type)) > 1) {
    stop(
      "Spectra must all be of the same type. ",
      "For more advanced inversions with multiple spectra types, ",
      "use `invert_bt` with a custom `model` argument."
    )
  }

  if (!is_spectra(spectra)) {
    stop(
      "`spectra` argument must be of class spectra. ",
      "See ?PEcAnRTM::spectra"
    )
  }

  rtm <- switch(
    type,
    `R` = function(param) prospect(param, prospect_version)[, 1],
    `T` = function(param) prospect(param, prospect_version)[, 2],
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

  if (test) {
    test_params <- prior$sampler()
    test_params <- test_params[-length(test_params)]
    test_model <- model(test_params)

    nr_obs <- ifelse(is.null(dim(observed)), length(observed), nrow(observed))
    if (nr_obs != nrow(test_model)) {
      stop(
        "Model and observation dimensions are not compatible. ",
        "Observation has ", nr_obs, " rows, but model has ",
        nrow(test_model), "."
      )
    }
  }

  invert_bt(
    observed = spectra,
    model = model,
    prior = prior,
    ...
  )

}
