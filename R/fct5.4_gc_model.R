#' G-Computation analysis
#'
#' @param dd a data frame
#' @param confounders a list of confounding factors
#' @param fu_time the follow-up time variable for right censored data
#' @param fu_event the status event indicator variable: 0 = alive/1 = dead, or T=death/F=alive or 1=alive/2=death
#' @param arm the 0/1 binary treatment arm variable
#' @param n_iter_gc the number of bootstrap resamples (100 by default) to estimate the variances and the confidence intervals
#' @param effect_gc the type of marginal effect to be estimated: could be "ATE" (by default), "ATT" or "ATU" (see details in gc.survival from RISCA package)
#'
#' @return the estimate of the marginal treatment effect from GC analysis
#' @export
#'
#' @examples
#' #to be completed
#'
fct5.4_gc_model <- function(dd, confounders, fu_time, fu_event, arm, n_iter_gc = 100, effect_gc = "ATE"){

  fmla_multi <- as.formula(paste0('survival::Surv(', fu_time,', ', fu_event,') ~ ', paste(c(arm, confounders), collapse = "+")))

  ## Cox model
  coxmodel <- coxph(fmla_multi, data = dd, x = TRUE) # for running gc.survival, we need to specify the argument x = TRUE in coxph regression

  ## GC analysis

  maxt <- max(dd[,fu_time], na.rm = TRUE)

  res <- RISCA::gc.survival(object = coxmodel,
                            data = dd,
                            group = arm,
                            times = fu_time,
                            failures = fu_event,
                            max.time = maxt,
                            effect = effect_gc,
                            iterations = n_iter_gc,
                            n.cluster = 1)

  out <- res$logHR %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(term = arm,
                  Nevent = coxmodel$nevent,
                  Model = "GC") %>%
    dplyr::relocate(term, .before = estimate)

  cat("done!\n")

  return(out)


}

