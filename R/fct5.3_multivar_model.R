#' Multivariable analysis
#'
#' @param dd a data frame
#' @param confounders a list of confounding factors
#' @param fu_time the follow-up time variable for right censored data
#' @param fu_event the status event indicator variable: 0 = alive/1 = dead, or T=death/F=alive or 1=alive/2=death
#' @param arm the 0/1 binary experimental arm variable
#'
#' @return the estimates of the conditional treatment effect from multivariable Cox model
#' @export
#'
#' @examples
#' #to be completed
#'
fct5.3_multivar_model <- function(dd, confounders, fu_time, fu_event, arm){

  fmla_multi <- as.formula(paste0('survival::Surv(', fu_time,', ', fu_event,') ~ ', paste(c(arm, confounders), collapse = "+")))

  ## Conditional analysis (Multivariable Cox model) --> We estimate the conditional effect of the treatment

  res <- survival::coxph(fmla_multi, data = dd)

  out <- res %>%
    broom::tidy(conf.int = TRUE, conf.level = 0.95) %>%
    dplyr::filter(term == arm) %>%
    dplyr::mutate(Nevent = res$nevent,
                  Model = "Multivar")

  return(out)

}
