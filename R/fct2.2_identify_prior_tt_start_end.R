#' Fonction qui filtre sur les critères relatifs aux TRT avec date de début et date de fin
#'
#' @param dv to be completed
#' @param dd to be completed
#' @param stdt to be completed
#' @param endt to be completed
#' @param delay to be completed
#'
#' @return to be completed
#' @export
#'
#' @examples
#' #to be completed
#'
#'
fct2.2_identify_prior_tt_start_end <- function(dv, dd, stdt, endt, delay){

  ## dv must be a long format data set that contains treatment/surgery/procedure/histology data and must contains "usubjid" = id name var
  ## dd must be a wide format data set and must contains: "usubjid" = id name var, "ARM" = treatment arm name var & "T0" = index date name var

  ## arm must be XP/CT format

  dv2 <- dv %>%

    ## Filtering id from external data set dv which are identified in our data set dd
    dplyr::filter(usubjid %in% dd$usubjid) %>%

    ## left_join dv and dd to get ARM and T0 variables from dd into dv
    dplyr::left_join(select(dd, usubjid, ARM, T0),by = "usubjid") %>%

    ## Identification of rows in dv compatible with procedure start date < T0 & T0 - procedure end date < X days: Identification of procedures/treatments/... that have been before before T0 and that ends more than X days before T0
    dplyr::mutate(delaystart = (as.Date(T0) - as.Date(!!as.symbol(stdt))),
           delaydt = (as.Date(T0) - as.Date(!!as.symbol(endt))),
           out = if_else((delaystart > 0) & (delaydt <= delay), 1, 0)) %>%

    ## Filtering rows in dv compatible with procedure start date < T0 & T0 - procedure end date < X days: Filtering procedures/treatments/... that have been before before T0 and that ends more than X days before T0
    dplyr::filter(out==1)

  ## Creating a new variable "excluded" that takes TRUE when IDs of dd are found in dv2: Identification of IDs in dd who had some procedures/treatments/... before before T0 and that ends more than X days before T0
  dd2 <- dd %>%
    dplyr::mutate(excluded = ifelse(usubjid %in% dv2$usubjid, TRUE, FALSE)) %>%
    dplyr::relocate(excluded, .after = "ARM")

  ## Announce how many IDs will be excluded
  cat(paste0(sum(dd2$excluded==TRUE), " units will be excluded \n"))

  ## Return data set that contains the new variable "excluded"
  return(dd2)

}




