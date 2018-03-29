#' Run BayesianTools sampler until convergence has been achieved
#'
#' @param bt_input Either previous BayesianTools output (class `mcmcsampler`) 
#' or a BayesianSetup object
#' @param common_settings BayesianTools settings list
#' @param first_settings Settings for first inversion attemp
#' @param loop_settings Settings for subsequent inversion attempts
#' @param sampler Character, which sampling algorithm to use. Default is "DEzs"
#' @param max_iter Maximum total number of iterations. If exceeded, return 
#' samples even if not converged. Default = 1e6.
#' @param min_samples Minimum number of samples to return after burnin. Even if 
#' converged, sampling will resume until this number is met. Default = 5000.
#' @param use_mpsrf Use multivariate PSRF in burnin calculations. Default = FALSE.
#' @param threshold PSRF convergence threshold. Default = 1.1
#' @param save_progress Target RDS file for saving samples between sampling 
#' steps. If `NULL` (default), don't save samples during runs.
#' @return BayesianTools `mcmcSampler` object
#' @export
sample_until_converged <- function(bt_input,
                                   common_settings = list(),
                                   first_settings = list(),
                                   loop_settings = list(),
                                   sampler = "DEzs",
                                   max_iter = 1e6,
                                   min_samples = 5000,
                                   use_mpsrf = FALSE,
                                   threshold = 1.1,
                                   save_progress = NULL
                                   ) {
  continue <- TRUE
  start_iter <- 1
  first <- TRUE
  repeat {
    if (first) {
      settings <- modifyList(common_settings, first_settings)
    } else {
      settings <- modifyList(common_settings, loop_settings)
      bt_input <- samples
    }
    if (is.null(settings$iterations)) {
      settings$iterations <- 10000
    }
    end_iter <- start_iter + settings$iterations - 1
    if (end_iter > max_iter) {
      message("End iteration ", end_iter, " exceeds max iterations ", max_iter,
              " but convergence has not been achieved.")
      break
    }
    message("Running iterations ", start_iter, " to ", end_iter)
    samples <- BayesianTools::runMCMC(
      bt_input,
      sampler = sampler,
      settings = settings
    )
    first <- FALSE
    if (!is.null(save_progress)) {
      saveRDS(samples, file = save_progress)
    }
    start_iter <- end_iter + 1
    converged <- bt_check_convergence(samples, threshold = threshold, use_mpsrf = use_mpsrf)
    if (!converged) {
      message("Convergence was not achieved. Resuming sampling.")
      next
    }
    coda_samples <- BayesianTools::getSample(samples, coda = TRUE)
    burned_samples <- PEcAn.assim.batch::autoburnin(
      coda_samples,
      threshold = threshold,
      return.burnin = TRUE,
      method = "gelman.plot"
    )
    if (burned_samples$burnin == 1) {
      message("PEcAn.assim.batch::autoburnin reports convergence has not been achieved. ",
              "Resuming sampling.")
      next
    }
    n_samples <- coda::niter(burned_samples$samples)
    if (n_samples < min_samples) {
      message(n_samples, " samples after burnin is less than target ", min_samples,
              ". Resuming sampling.")
      next
    }
    message("Done!")
    break
  }
  samples
}
