#' Model-error normal log-likelihood generator
#'
#' @param nparams Number of model parameters. Note that this is 1 or 2 fewer 
#' (depending on the error model) than the number of parameters passed to the 
#' sampler.
#' @param model Model function to call, whose outputs must line up with 
#' observations.
#' @param observed Observation vector or matrix. Must line up with dimensions 
#' of `model` output
#' @param sweep Logical. If `TRUE`, use `sweep` to calculate the model-dat 
#' difference, which allows dimension mismatch but is slower. Otherwise, use 
#' `+`.
#' @param verbose Logical. If `TRUE`, print verbose error messages when 
#' likelihood evaluation fails.
#' @param heteroskedastic Logical. If `TRUE`, residual SD is a linear function 
#' of the model output (note that this requires 2 residual parameters). If 
#' `FALSE` (default), it's a constant.
#' @return Log-likelihood function of a parameter vector, which can be used in 
#' a `bayesianSetup`
#' @export
rtm_loglike <- function(nparams, model, observed,
                        sweep = FALSE,
                        verbose = TRUE,
                        heteroskedastic = FALSE,
                        ...) {
  fail_ll <- -1e10
  stopifnot(
    nparams >= 1,
    nparams %% 1 == 0,
    is.function(model),
    is.numeric(observed)
  )
  n_obs <- length(observed)

  function(x) {
    rtm_params <- x[seq_len(nparams)]
    mod <- model(rtm_params, ...)
    if (any(is.na(mod))) {
      if (verbose) message(sum(is.na(mod)), " NA values in model output. Returning loglike = ", fail_ll)
      return(fail_ll)
    }
    if (sweep) {
      err <- sweep(-observed, 1, mod, "+")
    } else {
      err <- mod - observed
    }
    err2 <- err * err
    if (heteroskedastic) {
      a <- x[nparams + 1]
      b <- x[nparams + 2]
      rsd <- a + b * mod
    } else {
      rsd <- x[nparams + 1]
    }
    sigma2 <- rsd * rsd
    llv <- -0.5 * (n_obs * log(sigma2) + err2 / sigma2)
    ll <- sum(llv, na.rm = TRUE)
    if (all(is.na(llv))) {
      if (verbose) {
        message("Log likelihood is NA. Returning loglike = ", fail_ll)
        message("Mean error: ", mean(err))
        message("Mean Sigma2 = ", mean(sigma2))
      }
      return(fail_ll)
    }
    return(ll)
  }
}

#' Check convergence of BayesianTools output
bt_check_convergence <- function(samples, threshold = 1.1, use_CI = TRUE, use_mpsrf = TRUE) {
  i <- ifelse(use_CI, 2, 1)
  gelman <- try(BayesianTools::gelmanDiagnostics(samples))
  if (class(gelman) == 'try-error') {
    message('Error trying to calculate gelman diagnostic. Assuming no convergence')
    return(FALSE)
  }
  if (use_mpsrf) {
    gelman_vec <- c(gelman$psrf[,i], mpsrf = gelman$mpsrf)
  } else {
    gelman_vec <- gelman$psrf[,i]
  }
  exceeds <- gelman_vec > threshold
  if (any(exceeds)) {
    exceeds_vec <- gelman_vec[exceeds]
    exceeds_char <- sprintf('%s: %.2f', names(exceeds_vec), exceeds_vec)
    exceeds_str <- paste(exceeds_char, collapse = '; ')
    message('The following parameters exceed threshold: ', exceeds_str)
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Quick BayesianTools prior creator for PROSPECT model
#'
#' @param custom_prior List containing `param_name`, `distn`, `parama`, `paramb`, and `lower`
#' @inheritParams prospect
#' @inheritParams rtm_loglike
#' @export
prospect_bt_prior <- function(version, custom_prior = list(), heteroskedastic = FALSE) {
    col_names <- c('param_name', 'distn', 'parama', 'paramb', 'lower')
    prior_default_list <- list(
        N = list('N', 'norm', 1.4, 0.8, 1),
        Cab = list('Cab', 'lnorm', log(40), 0.9, 0),
        Car = list('Car', 'lnorm', log(10), 1.1, 0),
        Canth = list('Canth', 'lnorm', log(10), 1.1, 0),
        Cbrown = list('Cbrown', 'lnorm', log(1), 1.1, 0),
        Cw = list('Cw', 'lnorm', log(0.01), 1, 0),
        Cm = list('Cm', 'lnorm', log(0.009), 1, 0)
    )
    if (heteroskedastic) {
      prior_default_list$residual_intercept <-
        list('residual_intercept', 'norm', 0, 100, -Inf)
      prior_default_list$residual_slope <-
        list('residual_slope', 'norm', 0, 100, -Inf)
      rnames <- c("residual_intercept", "residual_slope")
    } else {
      prior_default_list$residual <-
        list('residual', 'lnorm', log(0.001), 2.5, 0)
      rnames <- "residual"
    }
    prior_list <- modifyList(prior_default_list, custom_prior)
    prior_df_all <- do.call(rbind.data.frame, prior_list)
    colnames(prior_df_all) <- col_names
    default_params <- defparam(paste0('prospect_', tolower(version)))
    use_names <- c(names(default_params), rnames)
    prior_df <- prior_df_all[prior_df_all[["param_name"]] %in% use_names, ]
    prior <- PEcAn.assim.batch::pda.create.btprior(prior_df)
    return(prior)
}
