
#' Fonction qui filtre sur la grace period
#'
#' @param dv to be completed
#' @param dd to be completed
#' @param diag_dt to be completed
#' @param delay1 to be completed
#' @param delay2 to be completed
#'
#' @return to be completed
#' @export
#'
#' @examples
#' #to be completed
#'
fct2.3_identify_tt_grace <- function(dv, dd, diag_dt, delay1, delay2){

  ## dv must be a long format data set that contains treatment/surgery/procedure/histology data and must contains "usubjid" = id name var
  ## dd must be a wide format data set and must contains: "usubjid" = id name var, "ARM" = treatment arm name var & "T0" = index date name var

  ## arm must be XP/CT format

  ## delay1 must be 0 or negative (before diag_dt) and delay2 must be zero or positive (after diag_dt).

  dv2 <- dv %>%

    ## Filtering id from external data set dv which are identified in our data set dd
    dplyr::filter(usubjid %in% dd$usubjid) %>%

    ## left_join dv and dd to get ARM and T0 variables from dd into dv
    dplyr::left_join(select(dd, usubjid, ARM, T0, diag_dt),by = "usubjid") %>%

    ## Identification of rows in dv compatible with treatment start date was more than X days (delay1) prior to diag_dt or more than X days (delay2) after diag_dt:
    ## Identification of procedures/treatments/... that have been initiated within the grace period
    dplyr::mutate(delaydt = (as.Date(T0) - as.Date(!!as.symbol(diag_dt))),
           out = ifelse((delaydt >= delay1) & (delaydt <= delay2), 0, 1)) %>%

    ## Filtering procedures/treatments/... that have been initiated wihtin the grace period
    dplyr::filter(out==1)

  ## Creating a new variable "excluded" that takes TRUE when IDs of dd are found in dv2: Identification of IDs in dd who initiated the treatments within the grace period
  dd2 <- dd %>%
    dplyr::mutate(excluded = ifelse(usubjid %in% dv2$usubjid, TRUE, FALSE)) %>%
    dplyr::relocate(excluded, .after = "ARM")

  ## Announce how many IDs will be excluded
  cat(paste0(sum(dd2$excluded==TRUE), " units will be excluded \n"))

  ## Return data set that contains the new variable "excluded"
  return(dd2)

}
