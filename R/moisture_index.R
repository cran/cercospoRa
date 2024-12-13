#' Relative humidity index
#'
#' Confirms the humidity threshold has been met by returning a 1 or not met by
#'  returning a 0. See Wolf and Verreet (2005)
#'
#' @param RH numeric, relative humidity as a percentage
#' @param rh_thresh numeric, humidity threshold where if humidity is below this
#'  no progress towards the latent period is made.
#' @param rain numeric, rainfall in millimetres
#'
#' @return numeric, either a 1 or 0
#' @noRd
#' @references
#' Wolf, P. F. J., and J. A. Verreet. “Factors Affecting the Onset of Cercospora
#'  Leaf Spot Epidemics in Sugar Beet and Establishment of Disease-Monitoring
#'  Thresholds.” *Phytopathology®* 95, no. 3 (March 2005): 269–74.
#'  https://doi.org/10.1094/PHYTO-95-0269.
moisture_index <- function(RH,
                           rain = 0,
                           rh_thresh = 70,
                           method = 1){
  dat2 <- data.table::data.table(RH = RH,
                                 rain = rain)
  rh_ind <- apply(dat2, 1, function(d2) {
    rain <- d2["rain"]
    RH <- d2["RH"]

    if (is.na(rain)){
      rain <- 0}

    # need to scale the threshold for more realistic values
    scale_rh_thresh <- (exp((140-rh_thresh)/21.7))/25

    if(method == 1){
    rh_ind <- data.table::fcase(rain >= 0.1, 1,
                                RH >= rh_thresh, 1,
                                default =  0)}
    if(method == 2){
      rh_ind <- data.table::fcase(rain >= 0.1, 1/(1+exp((88-RH)/(2.5 * scale_rh_thresh))),
                                  RH > rh_thresh, 1/(1+exp((88-RH)/(2.5 * scale_rh_thresh))),
                                  default =  0)}

    if(method != 1 &
       method != 2){ stop("argument 'method' needs to be '1' or '2' in reference to the
                         two methods used by wolf and verreet to calculate Mij")}

    return(rh_ind)
  })
  return(rh_ind)
}
