#' Descriptive analysis of the propensity scores and weights, and assessment of the common support
#'
#' @param dd a data frame
#' @param arm the 0/1 binary variable depending on whether the patients received either experimental trt or control trt
#' @param ps a variable that contains propensity score
#' @param weights a variable that contains a weight
#'
#' @return Summary statistics of propensity scores and weights, and return the distribution of propensity scores density plot
#' @export
#'
#' @examples
#' #to be completed
#'
fct4.1_propensity_overlap <- function(dd, arm, ps, weights){


  fmla <- as.formula(paste0(arm, " ~  ", paste(c(ps, weights), collapse = "+")))

    pssummary <- arsenal::tableby(
      fmla,
      data = dd,
      test=TRUE,
      total=TRUE,
      numeric.stats = c("N","Nmiss", "meansd", "medianq1q3", "iqr", "range"), numeric.test ="kwt",
      stats.labels = list(N ='N', Nmiss ='Missing', medianq1q3 ='Median (Q1, Q3)'))

    pssummary <- summary(pssummary, labelTranslations=c(arm = "Experimental treatment assignment", ps = "Propensity score", weights = "Weights"),
                         digits=2, title='Summary of propensity scores and weights', pfootnote = TRUE)

  fig <- as.data.frame(dd) %>%
    ## rename label of treatment arm
    dplyr::mutate(arm = if_else(get(arm) == 1, "Experimental treatment", "Standard treatment")) %>%

    ggplot(aes(x = ps, fill = forcats::fct_rev(arm))) +
    geom_density(alpha = .5) +
    labs(x = "Propensity Scores", y = "Density", fill = "") +
    ggtitle("Common Support of the probability of receiving the experimental treatment") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          text = element_text(size=14),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14))

  return(list("Density plot" = fig,
              "PS and weights summary" = pssummary))

}



