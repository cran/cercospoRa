#' Get temperature index
#'
#' Temperature index is a proportional representation of the latent period.
#' Temperatures at or above the optimum temperature for the disease cycle will
#' yield 1 and the lower the temperature the infinitely longer the latent period.
#'
#' For equations and original documentation, see Wolf and Verreet (2005)
#'  and Wolf et al. (2001)
#'
#' @param Tm numeric, temperature for any given hour
#' @param opt_Tm numeric, the lowest temperature optimum at which all temperatures
#'  above will have the same (fastest) latent period.
#'
#' @return numeric, proportion representing the speed of the latent period in relation
#'  to the temperature optimum
#' @references
#' Wolf, P. F. J., and J. A. Verreet. “Factors Affecting the Onset of Cercospora
#'  Leaf Spot Epidemics in Sugar Beet and Establishment of Disease-Monitoring
#'  Thresholds.” *Phytopathology®* 95, no. 3 (March 2005): 269–74.
#'  https://doi.org/10.1094/PHYTO-95-0269.
#'
#' Wolf, P. F. J., F.-J. Weis, and J.-A. Verreet. “Threshold Values as
#'  Indicators of Fungicide Treatments for the Control of Leaf Blotching Caused
#'  by Cercospora Beticola (Sacc.) in Sugar Beets.”
#'  *Journal of Plant Diseases and Protection* 108, no. 3 (2001): 244–57.

temperature_index <- function(Tm, opt_Tm = 21){

  out <- sapply(Tm,FUN = function(Tm_1){
    tm_ind <- latent_period(opt_Tm)/latent_period(Tm_1)
    if(tm_ind > 1) tm_ind <- 1
    if(tm_ind < 0) tm_ind <- 0

    return(tm_ind)
  })
  return(out)
}
