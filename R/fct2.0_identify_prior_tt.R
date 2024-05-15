#' Identify patients who received prior drugs before T0 (for instance, prior HT when it is not an exclusion criterion)
#'
#' @param dv to be completed
#' @param dd to be completed
#' @param dt to be completed
#'
#' @return to be completed
#' @export
#'
#' @examples
#' #to be completed
#'
#'
fct2.0_identify_prior_tt <- function(dv, dd, dt){

  ## dv must be a long format data set that contains treatment/surgery/procedure/histology data and must contains "usubjid" = id name var
  ## dd must be a wide format data set and must contains: "usubjid" = id name var, "ARM" = treatment arm name var & "T0" = index date name var

  ## arm must be XP/CT format

  dv2 <- dv %>%

    ## Filtering id from external data set dv which are identified in our data set dd
    dplyr::filter(usubjid %in% dd$usubjid) %>%

    ## left_join dv and dd to get ARM and T0 variables from dd into dv
    dplyr::left_join(select(dd, usubjid, ARM, T0), by = usubjid) %>%

    ## Identification of rows in dv compatible with T0 - procedure date > 0: Identification of procedures/treatments/... that have been before T0
    dplyr::mutate(delaydt = (as.Date(T0) - as.Date(!!as.symbol(dt))),
           prior = if_else((delaydt > 0), 1, 0)) %>%

    ## Filtering rows in dv compatible with T0 - procedure date > 0: Filtering procedures/treatments/... that have been before T0
    dplyr::filter(prior==1)

  ## Creating a new variable "excluded" that takes TRUE when IDs of dd are found in dv2: Identification of IDs in dd who had some procedures/treatments/... that have been before T0
  dd2 <- dd %>%
    dplyr::mutate(prior = ifelse(usubjid %in% dv2$usubjid, TRUE, FALSE)) %>%
    dplyr::relocate(prior, .after = "ARM")

  ## Announce how many IDs will be excluded
  cat(paste0(sum(dd2$prior==TRUE), " patients received prior drugs before T0 \n"))

  ## Return data set that contains the new variable "excluded"
  return(dd2)

}
