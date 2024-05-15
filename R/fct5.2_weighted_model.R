#' Weighted analysis
#'
#' @param dd a data frame
#' @param fu_time the follow-up time variable for right censored data
#' @param fu_event the status event indicator variable: 0 = alive/1 = dead, or T=death/F=alive or 1=alive/2=death
#' @param arm the 0/1 binary treatment arm variable
#' @param vname_weights the weights variable: could be "iptwate", "sw" or "psow" if propensity_weights function was used, or other
#'
#' @return the estimates of the marginal treatment effect from weighted Cox model
#' @export
#'
#' @examples
#' #to be completed
#'
fct5.2_weighted_model <- function(dd, fu_time, fu_event, arm, vname_weights){

  fmla_uni <- as.formula(paste0('survival::Surv(', fu_time,', ', fu_event,') ~ ', arm))

  #### IPTW-ATE weighted analysis (ATE-IPTW Cox model)

  #Run the weighted Cox model
  res <- survival::coxph(fmla_uni, weights = get(vname_weights), robust = TRUE, data = dd)

  out <- res %>%
    broom::tidy(conf.int = TRUE, conf.level = 0.95) %>%
    dplyr::filter(term == arm) %>%
    dplyr::mutate(Nevent = res$nevent,
                  Model = "Weighted")

  return(out)

}

