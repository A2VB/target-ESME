#' to be completed
#'
#' @param dd to be completed
#' @param n_midd to be completed
#' @param confounders to be completed
#' @param arm to be completed
#' @param vname_weights to be completed
#'
#' @return to be completed
#' @export
#'
#' @examples
#' #to be completed
#'

fct4.2_smd_table <- function(dd, n_midd=NULL, confounders, arm, vname_weights=NULL){

  dd1 <- dd %>%
    select(c(all_of(confounders), !!as.symbol(arm)))

  ## Stock names of numeric variables
  var_test_numeric <- unlist(lapply(dd1, is.numeric))
  var_numeric <- names(var_test_numeric[var_test_numeric])


  ## Matrix to treat all categorical variable as dichotomous variables
  dd1 <- model.matrix(  ~ . -1,   data = dd1)

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
  style_number_mi <- function(x) gtsummary::style_number(x, scale = 1/n_mi_dd)

  tab2 <- tab1 %>%

    gtsummary::tbl_summary(
      by=!!as.symbol(arm),

      type = list(
        all_of(var_factor) ~ "categorical",
        all_of(var_numeric) ~ "continuous"),

      digits = list(
        all_of(var_numeric) ~ c(1),  # Do not apply style_number to numeric variables
        all_of(var_factor) ~ list(style_number_mi, 0)) ## Do apply style_number() to factor variables
    ) %>%

    ## Add SMD for each variables
    gtsummary::add_difference(everything() ~ "smd")

  tab3 <- tab2 %>%
    gtsummary:: modify_header(update =
                                list(stat_1 ~ "**{level}**, N = {style_number_mi(n)}  ({style_percent(p, symbol = TRUE)})",
                                     stat_2 ~ "**{level}**, N = {style_number_mi(n)}  ({style_percent(p, symbol = TRUE)})"))

  return(tab3)

}



