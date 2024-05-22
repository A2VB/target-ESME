#' function that pool HRs estimates according to Bernard and Rubin's rules
#' cf : https://bookdown.org/mwheymans/bookmi/rubins-rules.html
#'
#' @param x to be completed
#' @param n_events to be completed
#' @param n_parameters to be completed
#' @param alpha to be completed
#'
#' @return to be completed
#' @export
#'
#' @examples
#' data(cars)
#' m1 <- lm(speed ~ dist, data = cars[sample(30, replace = TRUE),])
#' m2 <- lm(speed ~ dist, data = cars[sample(30, replace = TRUE),])
#' m3 <- lm(speed ~ dist, data = cars[sample(30, replace = TRUE),])
#' m4 <- lm(speed ~ dist, data = cars[sample(30, replace = TRUE),])
#' m5 <- lm(speed ~ dist, data = cars[sample(30, replace = TRUE),])
#' m <- list(m1, m2, m3, m4, m5)
#'
#' msum <- lapply(m, broom::tidy) %>%
#'   bind_rows() %>%
#'   filter(term == "dist")
#
#' fct6.0_Pool(msum, n_events = 30, n_parameters = 2)
#'
fct6.0_Pool <- function(x, n_events, n_parameters, alpha = 0.05){

  M <- nrow(x)
  Qm <- x$estimate      # paramètre d'intérêt (le log du HR par exemple) pour le dataset m
  Um <- x$std.error**2  # la variance du paramètre d'intérêt pour le dataset m

  # estimation poolée du paramètre = moyenne
  Q <- (1/M)*sum(Qm)
  Qe <- exp(Q) ## exp du bêta = HR

  # décomposition de l'estimation de la variance du paramètre poolé
  Vw <- (1/M)*sum(Um)              #U variance intra imputation (VW : variance within)
  Vb <- (1/(M-1))*sum((Qm - Q)**2) #B variance inter-imputation (VB : variance between)

  # variance totale de l'estimateur
  Vt <- Vw + Vb + Vb/M    # T : variance totale
  SE_pooled <- sqrt(Vt)

  # statistique de Wald pour l'estimateur poolé
  Wald_pooled <- abs(Q/SE_pooled) # wald on fait l'hypothèse de normalité mais valable quand on a bcp de données

  # degré de liberté pour IC (old Rubin version)
  Lambda <- (Vb + Vb/M)/Vt
  df_old <- (M - 1)/(Lambda**2) ## == (M - 1)*(1/( (1 + 1/M)*Vb/Vt)**2)
  q_old <- qt(1-alpha/2, df = df_old)
  binf_old <- Q - q_old*SE_pooled
  bsup_old <- Q + q_old*SE_pooled

  pvalue_old <- 2*(1 - stats::pt(q = Wald_pooled, df = df_old))

  df_obs <- (((n_events - n_parameters) + 1)/((n_events - n_parameters) + 3))*(n_events - n_parameters)*(1 - Lambda)
  df_adj <- (df_old * df_obs) / (df_old + df_obs)
  q_adj <- qt(1-alpha/2, df = df_adj)
  binf_adj <- Q - q_adj*SE_pooled
  bsup_adj <- Q + q_adj*SE_pooled
  pvalue_adj <- 2*(1 - stats::pt(q = Wald_pooled, df = df_adj))

  # Exp des bêta de l'IC ajustés (Barnard)
  lower <- exp(binf_adj)
  upper <- exp(bsup_adj)

  return(round(c("beta_pooled" = Q,
                 "se_beta_pooled" = SE_pooled,
                 "df_rubin" = df_old,
                 "IC95_inf_beta_pooled_rubin" = binf_old,
                 "IC95_sup_beta_pooled_rubin" = bsup_old,
                 "pvalue_beta_pooled_rubin" = pvalue_old,
                 "df_barnard" = df_adj,
                 "IC95_inf_beta_pooled_barnard" = binf_adj,
                 "IC95_sup_beta_pooled_barnard" = bsup_adj,
                 "pvalue_beta_pooled_barnard" = pvalue_adj,
                 "estimate" = Qe,
                 "lower" = lower,
                 "upper" = upper,
                 "p-value" = pvalue_adj
  ),7))
}

