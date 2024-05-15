#' Function that identifies patients who received the treatments we want to study
#'
#' @param dv to be completed
#' @param line to be completed
#' @param tt_xp1 to be completed
#' @param tt_xp2 to be completed
#' @param tt_xp3 to be completed
#' @param tt_xp4 to be completed
#' @param tt_xp5 to be completed
#' @param tt_ct1 to be completed
#' @param tt_ct2 to be completed
#' @param tt_ct3 to be completed
#' @param tt_ct4 to be completed
#' @param tt_ct5 to be completed
#' @param design to be completed
#'
#' @return to be completed
#' @export
#'
#' @examples
#' #to be completed
#'
#'
fct1.0_target_tt <- function(dv,
                           line = 1,
                           tt_xp1, tt_xp2 = NULL, tt_xp3 = NULL, tt_xp4 = NULL, tt_xp5 = NULL,
                           tt_ct1, tt_ct2 = NULL, tt_ct3 = NULL, tt_ct4 = NULL, tt_ct5 = NULL,
                           design = "A"){

  tt_xp <- c(tt_xp1, tt_xp2, tt_xp3, tt_xp4, tt_xp5)
  tt_ct <- c(tt_ct1, tt_ct2, tt_ct3, tt_ct4, tt_ct5)

  ## We need to choose the design:
  # A = XP: XP & CT; CT: CT alone
  # B = XP: XP alone or XP & CT; CT: CT alone
  # C = XP: XP alone ; CT: CT alone

  dd <- dv %>%

    ## Filtering only rows, i.e. the treatment records, according to the treatment line we chose
    dplyr::filter(lineid == line) %>%

    ## Filering the treatment strategies of interest
    dplyr::filter(mcdci_r %in% c(tt_xp, tt_ct)) %>%

    ## Group by the rows by ID
    dplyr::group_by(mcdci_r) %>%

    ## Identification of IDs concerning by the treatment strategies of interest
    ## by creating a new variable "arm" that takes 1 if ID is concerning by tt_xp, 2 if ID is concerning by tt_ct and 3 if not concerning by neither
    dplyr::mutate(arm =
             if_else(all(tt_xp %in% mcdci_r), 1,
                     if_else(all(tt_ct %in% mcdci_r), 2, 3))) %>%

    ## ungroup rows
    dplyr::ungroup() %>%

    ## Ranking rows, i.e. the treatments records by start and end dates
    dplyr::mutate(rankf = paste0('drug_', rankf)) %>%
    tidyr::pivot_wider(names_from = rankf,
                       values_from = c(mcdci_r, start, end)) %>%

    ## Identification of IDs who received the treatment strategies according to the chosen design
    ## by creating a new variable "ARM" that takes XP if concerning by tt_xp according to the design, CT if concerning by tt_ct according to the design and EXCLUDED if receiving independently to the design
    dplyr::mutate(design = design,
           ARM = case_when(
             design == "A" & arm == 1 ~ "XP",
             design == "A" & arm == 2 ~ "CT",
             design == "A" & arm == 3 ~ "EXCLUDED",

             design == "B" & arm == 1 ~ "XP",
             design == "B" & arm == 2 ~ "CT",
             design == "B" & arm == 3 ~ "XP",

             design == "C" & arm == 1 ~ "EXCLUDED",
             design == "C" & arm == 2 ~ "CT",
             design == "C" & arm == 3 ~ "XP",

             TRUE ~ "else"
           )) %>%
    dplyr::relocate(ARM, .after=arm)

  ## Derive Time Zero as the min start date of treatment
  dd$T0 <- apply(dplyr::select(dd, starts_with("start")), 1, min, na.rm=TRUE)

  dd_post <- dd %>%
    dplyr::filter(ARM == "XP" | ARM == "CT")# %>%
    #dplyr::relocate(T0, .after=ARM)


  ## Get no. excluded patients
  ## = no. of patients in prior data set - no. of pts in post data set (receiving drugs we want to study)  + no. of pts excluded according to the chosen design
  n_excluded_first = length(unique(dv$usubjid)) - nrow(dd)
  n_excluded_design = sum(dd$ARM=="EXCLUDED")
  n_excluded_total = length(unique(dv$usubjid)) - nrow(dd) + sum(dd$ARM=="EXCLUDED")

  cat(paste0(n_excluded_first, " patients are excluded because they did not receive the treatments we want to study", "\n",
             n_excluded_design, " patients are excluded because they did not receive the treatments according to the chosen design", "\n",
             n_excluded_total, " patients are excluded in total"))

  return(dd_post)

}


