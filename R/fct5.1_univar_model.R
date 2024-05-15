#' Univariable/Naïve analysis
#'
#' @param dd a data frame
#' @param fu_time the follow-up time variable for right censored data
#' @param fu_event the status event indicator variable: 0 = alive/1 = dead, or T=death/F=alive or 1=alive/2=death
#' @param arm the 0/1 binary experimental arm variable
#'
#' @return the estimate of the crude treatment effect from univariable Cox model
#' @export
#'
#' @examples
#' #to be completed
#'
fct5.1_univar_model <- function(dd, fu_time, fu_event, arm){

  ## Crude analysis (univariable/raw Cox model) --> We estimate the raw effect of the treatment

  fmla_uni <- as.formula(paste0('survival::Surv(', fu_time,', ', fu_event,') ~ ', arm))

  res <- survival::coxph(fmla_uni, data = dd)

  out <- res %>%
    broom::tidy(conf.int = TRUE, conf.level = 0.95) %>%
    dplyr::filter(term == arm) %>%
    dplyr::mutate(Nevent = res$nevent,
                  Model = "Univar")

  return(out)

}
