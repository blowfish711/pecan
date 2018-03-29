#' Perform Bayesian inversion using BayesianTools package
#'
#' Use samplers from the BayesianTools package to fit models to data. Like
#' `invert.auto`, this will continue to run until convergence is achieved
#' (based on Gelman diagnostic) _and_ the result has enough samples (as
#' specified by the user; see Details).
#'
#' @details `custom_settings` is a list of lists, containing the following:
#'  * `common` -- BayesianTools settings common to both the initial and subsequent samples.
#'  * `init` -- BayesianTools settings for just the first round of sampling.
#'  This is most common for the initial number of iterations, which is the
#'  minimum expected for convergence.
#'  * `loop` -- BayesianTools settings for iterations inside the convergence
#'  checking `while` loop. This is most commonly for setting a smaller
#'  iteration count than in `init`.
#'  * `other` -- Miscellaneous (non-BayesianTools) settings, including:
#'      - `sampler` -- String describing which sampler to use. Default is `DEzs`
#'      - `use_mpsrf` -- Use the multivariate PSRF to check convergence.
#'      Default is `FALSE` because it may be an excessively conservative
#'      diagnostic.
#'      - `min_samp` -- Minimum number of samples after burnin before stopping.
#'      Default is 5000.
#'      - `max_iter` -- Maximum total number of iterations. Default is 1e6.
#'      - `save_progress` -- File name for saving samples between loop
#'      iterations. If `NULL` (default), do not save progress samples.
#'      - `threshold` -- Threshold for Gelman PSRF convergence diagnostic. Default is 1.1.
#'      - `verbose_loglike` -- Diagnostic messages in log likelihood output. Default is TRUE.
#'      - `sweep` -- If `TRUE`, use `sweep` to calculate error in likelihood.  
#'      If `FALSE`, use direct subtraction, which is faster, but may fail if 
#'      elements have different sizes. If `NULL` (default), figure this out by 
#'      running `model` and comparing to `observed`.
#'      - `heteroskedastic` -- If `TRUE`, use heteroskedastic variance in 
#'      likelihood
#'
#' See the BayesianTools sampler documentation for what can go in the `BayesianTools` settings lists.
#'
#' Under the hood, this calls [sample_until_converged].
#' @param observed Vector of observations. Ignored if `loglike` is not `NULL`.
#' @param model Function called by log-likelihood. Must be `function(params)`
#' and return a vector equal to `length(observed)` or `nrow(observed)`. Ignored 
#' if `loglike` is not `NULL`.
#' @param prior BayesianTools prior object.
#' @param custom_settings Nested settings list. See Details.
#' @param loglike Custom log likelihood function. If `NULL`, use [rtm_loglike] 
#' with provided `observed` and `model`.
#' @param test Logical. If `TRUE`,  run the log likelihood once to make sure it 
#' doesn't return an error before starting sampling. Default = `FALSE`.
#' @export
invert_bt <- function(observed, model, prior, custom_settings = list(), loglike = NULL, test = FALSE) {

  default_settings <- list(
    common = list(),
    init = list(iterations = 10000),
    loop = list(iterations = 2000),
    other = list(
      sampler = 'DEzs',
      use_mpsrf = FALSE,
      min_samp = 5000,
      max_iter = 1e6,
      save_progress = NULL,
      threshold = 1.1,
      verbose_loglike = TRUE,
      sweep = NULL,
      heteroskedastic = FALSE
    )
  )

  if (length(custom_settings) > 0) {
    settings <- list()
    for (s in seq_along(default_settings)) {
      s_name <- names(default_settings)[s]
      if (s_name %in% names(custom_settings)) {
        settings[[s_name]] <- modifyList(default_settings[[s_name]],
                                         custom_settings[[s_name]])
      } else {
        settings[[s_name]] <- default_settings[[s_name]]
      }
    }
  } else {
    settings <- default_settings
  }

  use_mpsrf <- settings[['other']][['use_mpsrf']]
  min_samp <- settings[['other']][['min_samp']]
  max_iter <- settings[['other']][['max_iter']]
  save_progress <- settings[['other']][['save_progress']]
  threshold <- settings[['other']][['threshold']]
  verbose_loglike <- settings[['other']][['verbose_loglike']]
  sweep <- settings[["other"]][["sweep"]]
  heteroskedastic <- settings[["other"]][["heteroskedastic"]]

  if (!is.null(save_progress)) {
    # `file.create` returns FALSE if target directory doesn't exist.
    stopifnot(file.create(save_progress))
  }
  stopifnot('prior' %in% class(prior))
  test_samp <- prior$sampler()
  param_names <- names(test_samp)
  resnames <- "residual"
  if (heteroskedastic) {
    resnames <- paste0(resnames, c("_slope", "_intercept"))
  }
  if (is.null(param_names)) {
    warning("Parameters are not named! Unable to check validity.")
  } else {
    if (!all(resnames %in% param_names)) {
      stop("One of the parameters must be `residual`.")
    }
  }
  nparams <- length(test_samp[!param_names %in% resnames])

  if (is.null(sweep)) {
    observed_is_matrix <- !is.null(dim(observed))
    n_obs <- ifelse(observed_is_matrix, length(observed), nrow(observed))
    try_model <- model(prior$sampler()[seq_len(nparams)])
    if (is.null(dim(try_model))) {
      # Model returns a vector. Arithmetic should work.
      matching_dims <- n_obs == length(try_model)
      sweep <- FALSE
    } else if (observed_is_matrix && ncol(try_model) == ncol(observed)) {
      # Aligned matrices. Arithmetic should still work.
      matching_dims <- n_obs == nrow(try_model)
      sweep <- FALSE
    } else if (observed_is_matrix && ncol(try_model) == 1) {
      #message(
        #"Model returns an N x 1 matrix but observed is N x M matrix. ",
        #"Likelihood will use `sweep` to calculate residual error, ",
        #"which is slower than direct arithmetic. ",
        #"If possible, convert `model` output to a vector."
      #)
      sweep <- TRUE
      matching_dims <- TRUE
    } else {
      matching_dims <- FALSE
    }
    if (!matching_dims) {
      stop("Dimension mismatch between observation and model.")
    }
  }

  if (is.null(loglike)) {
    loglike <- rtm_loglike(
      nparams = nparams,
      model = model,
      observed = observed,
      verbose = verbose_loglike,
      sweep = sweep,
      heteroskedastic = heteroskedastic
    )
  }

  if (test) {
    try_model <- tryCatch(
      loglike(prior$sampler()),
      warning = function(w) stop("Warning in log-likelihood function: ", w),
      error = function(e) stop("Error in log-likelihood function: ", e)
    )
  }

  setup <- BayesianTools::createBayesianSetup(
    likelihood = loglike,
    prior = prior,
    names = param_names
  )

  samples <- sample_until_converged(
    setup,
    settings$common,
    first_settings = settings$init,
    loop_settings = settings$loop,
    sampler = settings$other$sampler,
    max_iter = max_iter,
    min_samples = min_samp,
    use_mpsrf = use_mpsrf,
    threshold = threshold,
    save_progress = save_progress
  )

  samples
}
