#' Return propensity scores and weights
#'
#' @param dd a data frame
#' @param confounders a list of confounding factors
#' @param arm the 0/1 binary experimental arm
#' @param w_method can take the value "iptwate" (IPTW-ATE), "sw" (SIPTW), or "psow" (PSOW)
#'
#' @return to be completed
#' @export
#'
#' @examples
#' #to be completed
#'
fct4.0_propensity_weights <- function(dd, confounders, arm, w_method){


  ## Fit binary logistic regression model for propensity score
  fmla1 <- as.formula(paste0(arm, " ~  ", paste(confounders, collapse = "+")))

  ps_model <- glm(fmla1, family = binomial(link="logit"), data = dd)

  ## Fit binary logistic regression model for marginal probability
  # fmla2 <- as.formula(I(arm == "PBVZ") ~ 1)
  fmla2 <- as.formula(I(get(arm) == 1) ~ 1)
  marg_model <- glm(fmla2, family = "binomial", data = dd)

  if(w_method == "iptwate") {

    ## Estimate the PSs and the marginal probability to compute w_method
    dd <- dd %>%
      dplyr::mutate(
        ## Generate logit of PSs and the PSs
        ps_logit = predict(ps_model, type = "link"),
        ps = predict(ps_model, type = "response"),

        ## Compute Stabilised weights
        pdenom = dplyr::if_else(get(arm) != 1, 1-ps, ps),

        # IPTW-ATE
        iptwate = 1/pdenom
      )

  }

  else if (w_method == "sw") {

    ## Estimate the PSs and the marginal probability to compute weights
    dd <- dd %>%
      dplyr::mutate(
        ## Generate logit of PSs and the PSs
        ps_logit = predict(ps_model, type = "link"),
        ps = predict(ps_model, type = "response"),

        ## Generate logit of marginal probability and marginal probability
        mar_logit = predict(marg_model, type = "link"),
        marginal = predict(marg_model, type = "response"),

        ## Compute w_method
        # Stabilised w_method numerator
        pnum = dplyr::if_else(get(arm) != 1, 1-marginal, marginal),

        # Stabilised w_method denominator
        pdenom = dplyr::if_else(get(arm) != 1, 1-ps, ps),

        # Stabilised w_method (SIPTW w_method)
        sw = pnum/pdenom

      )

  }


  else if (w_method == "psow") {

    ## Estimate the PSs and the marginal probability to compute w_method
    dd <- dd %>%
      dplyr::mutate(
        ## Generate logit of PSs and the PSs
        ps_logit = predict(ps_model, type = "link"),
        ps = predict(ps_model, type = "response"),

        ## Compute w_method
        # Overlap w_method (PSOW w_method)
        psow = dplyr::if_else(get(arm) != 1, ps, 1-ps)

      )

  }
  return(dd)

}

