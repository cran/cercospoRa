#' Calculate cDIV threshold for sugar beet cultivars
#'
#' @description
#' This function was written using summary data from Wolf and Verreet (2005),
#'  Table 1 and Table 2 to determine the equivalent cumulative daily infection
#'  value where epidemic onset begins. The average difference between 'high' and
#'  'low' susceptible cultivars was 1.2 on the variety scale. The average
#'  difference between low and highly susceptible cultivars minimum cDIV on each
#'  starting time in Table 2 was 4.637.
#'
#' @param var_scale numeric, bsa scale (1-9) of the sugar beet cultivars.
#'  1 is the lowest susceptibility and 9 is the highest susceptibility.
#'  See https://www.bundessortenamt.de and Wolf and Verreet (2005) Table 1.
#'
#' @returns numeric, cDIV threshold for the start of the epidemic
#'
calc_susceptibility <- function(var_scale = 4){
  if(var_scale < 1 | var_scale > 9){
    stop("Variety scale must be between 1 (lowest susceptibility) and 9, (highest
         susceptibility)")
  }

  if(var_scale >= 4){
    sus_coef <-  (var_scale - 4)^(1/2)}
  else{
    sus_coef <-  -(abs(var_scale - 4)^(1/2))
    }

  cDIV_threshold <- 12.4724 - sus_coef * 4.6431

  return(cDIV_threshold)

}
