#' Calculate Cercospora Latent Period
#'
#' @details Calculates the latent period for *Cercospora beticola* infections on
#' sugar beet. Note the published formula in Wolf and Verreet (2005)
#' contains an error in the exponent. e(0.35 x (Tm - 10)) should be
#' e(-0.35 x (Tm - 10)). See issue #22 on Github for additional information.
#'
#' @param Tm numeric, Average temperature in degrees Celsius for a hour of the day
#' @param ref character, method for calculating latent period. Default is
#'  \code{"wolf"} also available \code{"jarroudi"}. See references for where
#'  formulas were used
#'
#' @return numeric, Latent period for a given hour. \code{ref = "jarroudi"}
#'  returns latent period in days.
#' @references
#' Wolf, P. F. J., and J. A. Verreet. “Factors Affecting the Onset of Cercospora
#'  Leaf Spot Epidemics in Sugar Beet and Establishment of Disease-Monitoring
#'  Thresholds.” *Phytopathology®* 95, no. 3 (March 2005): 269–74.
#'  https://doi.org/10.1094/PHYTO-95-0269.
#'
#' El Jarroudi, Moussa, Fadia Chairi, Louis Kouadio, Kathleen Antoons,
#'  Abdoul-Hamid Mohamed Sallah, and Xavier Fettweis. “Weather-Based Predictive
#'  Modeling of Cercospora Beticola Infection Events in Sugar Beet in Belgium.”
#'  *Journal of Fungi* 7, no. 9 (September 18, 2021): 777.
#'  https://doi.org/10.3390/jof7090777.

latent_period <- function(Tm, ref = "wolf"){
  if(ref == "wolf"){
  lp <-
    sapply(Tm, function(Tm) {
      7 + 26 * exp(-0.35 * (Tm - 10))
    })
  }
  if(ref == "jarroudi"){
    lp <-
      sapply(Tm,function(Tm){
      lper <-
        1/(0.00442 * Tm - 0.0238)
      if(lper < 0) lper <- 0
      return(lper)
    })
  }

  return(lp)
}
