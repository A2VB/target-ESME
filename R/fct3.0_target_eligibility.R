#' Function that filters patients based on baseline inclusion and/or exclusion criteria
#'
#' @param dd to be completed
#' @param criter_in to be completed
#' @param criter_ex to be completed
#' @param ARM to be completed
#'
#' @return to be completed
#' @export
#'
#' @examples
#' #to be completed
#'
fct3.0_target_eligibility <- function(dd, criter_in = NULL, criter_ex = NULL, ARM = "ARM"){

  if(!is.null(criter_in)){

    ## Apply inclusion criteria

    table_in <- lapply(criter_in,
                       function(crit){

                         ## Get no. of rows prior filtering among all and per arm
                         n_prior <- nrow(dd)
                         n_prior_CT <- sum(dd[,ARM]==0)
                         n_prior_XP <- sum(dd[,ARM]==1)

                         ## Define filtering string that is criterion name
                         criter_string <- paste0("(", crit, ")")

                         ## Filtering on the inclusion criterion
                         subset_dd <- subset(x = dd,
                                  subset = eval(parse(text = criter_string))) ## subset pts with crit == TRUE

                         ## Get no. of rows post filtering among all and per arm
                         n_post <- nrow(subset_dd)
                         n_post_CT <- sum(subset_dd[,ARM]==0)
                         n_post_XP <- sum(subset_dd[,ARM]==1)

                         ## Save data set with only observations compatible with the inclusion criterion
                         dd <<- subset_dd

                         ## Create table
                         table <- data.frame(Category   = "Inclusion",
                                             Criterion  =  crit,
                                             N_prior      = n_prior,
                                             N_post       = n_post,
                                             N_excluded   = n_prior - n_post,

                                             N_prior_CT    = n_prior_CT,
                                             N_post_CT     = n_post_CT,
                                             N_excluded_CT = n_prior_CT - n_post_CT,

                                             N_prior_XP    = n_prior_XP,
                                             N_post_XP     = n_post_XP,
                                             N_excluded_XP = n_prior_XP - n_post_XP
                                             )

                         return(table)

                       }) %>%
      dplyr::bind_rows()


    ## Add total no. of included/excluded observations
    table_in <- rbind(table_in,
                      data.frame(Category   = "Total",
                                 Criterion  =  "All",
                                 N_prior    =  max(table_in$N_prior),
                                 N_post     =  min(table_in$N_post),
                                 N_excluded =  max(table_in$N_prior) - min(table_in$N_post),

                                 N_prior_CT    =  max(table_in$N_prior_CT),
                                 N_post_CT     =  min(table_in$N_post_CT),
                                 N_excluded_CT =  max(table_in$N_prior_CT) - min(table_in$N_post_CT),

                                 N_prior_XP    =  max(table_in$N_prior_XP),
                                 N_post_XP     =  min(table_in$N_post_XP),
                                 N_excluded_XP =  max(table_in$N_prior_XP) - min(table_in$N_post_XP)

                      ))


  }



  ## Apply exclusion criteria
  if(!is.null(criter_ex)){

    table_ex <- lapply(criter_ex,
                       function(crit){

                         # Get no. of rows prior filtering among all and per arm
                         n_prior <- nrow(dd)
                         n_prior_CT <- sum(dd[,ARM]==0)
                         n_prior_XP <- sum(dd[,ARM]==1)

                         # Define filtering string that is criterion name
                         criter_string <- paste0("!(", crit, ")")

                         # Filtering on the inclusion criterion
                         subset_dd <-
                           subset(x = dd,
                                  subset = eval(parse(text = criter_string))) ## subset pts with crit != TRUE, i.e. we exclude pts with crit == TRUE

                         # Get no. of rows post filtering among all and per arm
                         n_post <- nrow(subset_dd)
                         n_post_CT <- sum(subset_dd[,ARM]==0)
                         n_post_XP <- sum(subset_dd[,ARM]==1)

                         # Save data set with only observations compatible with the exclusion criterion
                         dd <<- subset_dd

                         # Create table
                         table <- data.frame(Category   = "Exclusion",
                                             Criterion  =  crit,
                                             N_prior    =  n_prior,
                                             N_post     =  n_post,
                                             N_excluded =  n_prior - n_post,

                                             N_prior_CT    =  n_prior_CT,
                                             N_post_CT     =  n_post_CT,
                                             N_excluded_CT =  n_prior_CT - n_post_CT,

                                             N_prior_XP    =  n_prior_XP,
                                             N_post_XP     =  n_post_XP,
                                             N_excluded_XP =  n_prior_XP - n_post_XP
                                             )

                         return(table)

                       }) %>%
      dplyr::bind_rows()

    # Add total no. of included/excluded observations
    table_ex <- rbind(table_ex,
                      data.frame(Category   = "Total",
                                 Criterion  =  "All",
                                 N_prior    = max(table_ex$N_prior),
                                 N_post     = min(table_ex$N_post),
                                 N_excluded = max(table_ex$N_prior) - min(table_ex$N_post),

                                 N_prior_CT    = max(table_ex$N_prior_CT),
                                 N_post_CT     = min(table_ex$N_post_CT),
                                 N_excluded_CT = max(table_ex$N_prior_CT) - min(table_ex$N_post_CT),

                                 N_prior_XP    = max(table_ex$N_prior_XP),
                                 N_post_XP     = min(table_ex$N_post_XP),
                                 N_excluded_XP = max(table_ex$N_prior_XP) - min(table_ex$N_post_XP)
                      ))
  }


  ## Return data set after inclusions/exclusions and table with no. inclusions/exclusions

  if(!is.null(criter_in) & is.null(criter_ex)) {

    out <- list(table_in = table_in,
                dataset  = dd)

  } else if(is.null(criter_in) & !is.null(criter_ex)) {

    out <- list(table_ex = table_ex,
                dataset  = dd)

  } else if(!is.null(criter_in) & !is.null(criter_ex)) {

    table_final <- rbind(table_in[-nrow(table_in), ], table_ex)

    out <- list(table_final = table_final,
                dataset = dd)

  } else {
    NULL
  }


  cat("done!\n")

  ## Return output
  return(out)


}
