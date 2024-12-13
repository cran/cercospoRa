#' Calculate daily infection values
#'
#' This function calculates the daily infection values for *Cercospora beticola*
#'  on sugar beet. Functions were adapted from Wolf and Verreet (2005)
#'  and Wolf et al (2001)
#'
#' @param date_time POSIX_ct, date time the weather recording was taken
#' @param Tm numeric, temperature, in Celsius' at time increment in
#'  \code{date_time}
#' @param RH numeric, relative humidity (%) at time increment in \code{date_time}
#' @param rain numeric, volume of rain in millimetres recorded between time recordings
#' @param dat data.frame, containing column names "times","temp","rh","rain" with
#'  each of the respective arguments for input. provided as a convenience
#'
#' @return data.table, with probability of infection for each day, between 0 and 1
#'  Undertaken with two methods by Wolf and Verreet (2005)
#' @export
#' @references
#' Wolf, P. F. J., and J. A. Verreet. “Factors Affecting the Onset of Cercospora
#'  Leaf Spot Epidemics in Sugar Beet and Establishment of Disease-Monitoring
#'  Thresholds.” *Phytopathology®* 95, no. 3 (March 2005): 269–74.
#'  https://doi.org/10.1094/PHYTO-95-0269.
#'
#' Wolf, P. F. J., M. Heindl, and J. A. Verreet. “Influence of Sugar Beet Leaf Mass
#'  Development on Predisposition of the Crop to Cercospora Beticola (Sacc.).”
#'  *Journal of Plant Diseases and Protection* 108, no. 6 (2001): 578–92.
#'
#' @examples
#' date_t <- Sys.time() + seq(0, 179 * 60 * 10, (60 * 10))
#' Tm <- rnorm(180,20,10)
#' RH <- runif(180,min = 40,90)
#' rain <- rbinom(180,1,0.1) * runif(180,0.1,20)
#'
#' DIV1 <- calc_DIV(
#' date_time = date_t,
#' Tm = Tm,
#' RH = RH,
#' rain = rain
#' )
calc_DIV <- function(date_time, Tm,RH, rain,dat){
  # declare non-globals
  times <- temp <- rh <- Tm_index <- moist_ind <- s_rate <- inf_rate <- NULL

  # check if dat is not supplied and create new data.table
  if(missing(dat)){
    if(missing(rain)) rain <- rep(0,length(RH))
    dat <- data.table::data.table(times = date_time,
                                  temp = Tm,
                                  rh = RH,
                                  rain = rain)
  }
  if(all(c("times","temp","rh","rain") %in% colnames(dat)) == FALSE){
    stop("'dat' data.frame must have colnames 'times','temp','rh','rain'")
  }

  data.table::setDT(dat)
  # Add columns for time increments for aggregation
  dat[,c("Year",
         "Month",
         "Day",
         "Hour") := list(data.table::year(times),
                         data.table::month(times),
                         data.table::mday(times),
                         data.table::hour(times))]
  # Aggregate to hourly
  dat <- dat[, c("temp",
                 "rh",
                 "rain") := list(mean(temp),
                                 mean(rh),
                                 sum(rain)),
             by = c("Year","Month","Day","Hour")
             ]

  dat[, c("Tm_index",
          "moist_ind") := list(temperature_index(temp),
                              moisture_index(rh,rain,70))]

  DIV <- dat[, list(DIV = round(mean(fifelse(test = Tm_index == 0 | moist_ind == 0,
                                  yes = 0, no = Tm_index * moist_ind)),digits = 7)),
             by = c("Year", "Month", "Day")]

  return(DIV)
}
