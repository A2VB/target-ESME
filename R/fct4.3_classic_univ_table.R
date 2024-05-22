
#' to be completed
#'
#' @param dd  to be completed
#' @param n_midd  to be completed
#' @param confounders  to be completed
#' @param arm  to be completed
#' @param vname_weights  to be completed
#' @param label  to be completed
#'
#' @return  to be completed
#' @export
#'
#' @examples
#' #to be completed
#'

fct4.3_classic_univ_table <- function(dd, n_midd=NULL, confounders, arm, vname_weights=NULL, label = NULL){


  dd1 <- dd %>%
    select(c(all_of(confounders), !!as.symbol(arm)))

  ## Stock names of numeric variables
  var_test_numeric <- unlist(lapply(dd1, is.numeric))
  var_numeric <- names(var_test_numeric[var_test_numeric])

  ## Table of characteristics on average across all imputed datasets with SMDs
  tab1 <- as.data.frame(dd1) %>%
    mutate(!!as.symbol(arm) := case_when(
      !!as.symbol(arm) == 0 ~ "Control treatment",
      !!as.symbol(arm) == 1 ~ "Experimental treatment"))

  ## Stock names of factor variables
  var_factor <- names(tab1)[!names(tab1) %in% var_numeric]

  ## Stock n_midd parameter defined by the user
  n_mi_dd <- n_midd

  ## Function that divides by n_midd each no. of observation in order to have the average no. across all imputed datasets
  ## If dd is not a list of datasets, but a unique dataset, then use n_midd=1
  style_number_mi <- function(x) gtsummary::style_number(x, scale = 1/n_mi_dd)  ## divided by n_midd is equal to multiply by 1/n_midd)


  if(!is.null(label)){

  tab2 <- tab1 %>%

    gtsummary::tbl_summary(
      by=!!as.symbol(arm),

      type = list(
        all_of(var_factor) ~ "categorical",
        all_of(var_numeric) ~ "continuous2"),

      statistic = all_of(var_numeric)~ c(
        "{mean} ({sd})",
        "{median} ({p25}, {p75})"),

      label = label,

      digits = list(
        all_of(var_numeric) ~ c(1),  # Do not apply style_number to numeric variables
        all_of(var_factor) ~ list(style_number_mi, 0)) ## Do apply style_number() to factor variables

    )


  }

  if(is.null(label)){

    tab2 <- tab1 %>%

      gtsummary::tbl_summary(
        by=!!as.symbol(arm),

        type = list(
          all_of(var_factor) ~ "categorical",
          all_of(var_numeric) ~ "continuous2"),

        statistic = all_of(var_numeric) ~ c(
          "{mean} ({sd})",
          "{median} ({p25}, {p75})"),


        digits = list(
          all_of(var_numeric) ~ c(1),  # Do not apply style_number to numeric variables
          all_of(var_factor) ~ list(style_number_mi, 0)) ## Do apply style_number() to factor variables

      )


  }


  ## Modify header to add % after N by level, and divide N by n_midd
  tab3 <- tab2 %>%
    gtsummary:: modify_header(update =
                                list(stat_1 ~ "**{level}**, N = {style_number_mi(n)}  ({style_percent(p, symbol = TRUE)})",
                                     stat_2 ~ "**{level}**, N = {style_number_mi(n)}  ({style_percent(p, symbol = TRUE)})"))



  return(tab3)

}





