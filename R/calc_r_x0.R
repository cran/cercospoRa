#' Calculate growth rate and LAI at t0
#'
#' @details
#' Fits a non-linear model to remotely sensed LAI values and estimates the leaf
#' area index (LAI) and growth rate _r_ at the start of the time window _t0_.
#'
#'
#' @param param_r Output of the function \link[cercospoRa]{read_sb_growth_parameter},
#'  which produces a list containing the LAI images and the associated dates
#' @param min_r minimum growth rate for sugar beet. Default \code{min_r} is fixed
#'  to 0.02 to ensure that the growth rate at the inflection point of the sigmoid
#'  is at least 1 unit of LAI per month.
#' @param max_r maximum growth rate for sugar beet. Default \code{max_r} is fixed
#'  to 0.05 to ensure that the growth rate at the inflection point of the sigmoid
#'  is at most 2.5 units of LAI per month.
#' @param k carrying capacity, which is the maximum LAI that can be attained.
#'  This value can be cultivar-dependent. The default is fixed to 6
#'
#' @return param_rxt: list containing parameters that are necessary to calculate
#'  canopy closure dates. These parameters are \code{r}, the growth rate;
#'  \code{x0}, the initial LAI value; and \code{t0}, the initial date.
#'
#' @export
#'
#' @examples
#' img_dir <- system.file("extdata", "uav_img",package = "cercospoRa")
#' epidemic_onset_param <-
#'   read_sb_growth_parameter(img_files = list.files(img_dir,pattern = "tif",
#'                                                   full.names = TRUE),
#'                            img_dates = as.POSIXct(
#'                              c("2022-06-14","2022-06-28"),tz = "UTC"),
#'                            target_res = 10)
#' param_rxt <- calc_r_x0(epidemic_onset_param,
#'                        min_r = 0.02,
#'                        max_r = 0.05,
#'                        k = 6)
calc_r_x0 <- function(param_r,
                      min_r = 0.02,
                      max_r = 0.05,
                      k = 6){
  tm <- as.POSIXct(names(param_r), tz = "UTC")
  tm <- as.numeric(tm)/(24*60*60)
  t0 <- tm[1]

  x0 <- terra::app(param_r, fit_rx, k = k, tm = tm, t0 = t0, rtn = "x0")
  r <- terra::app(param_r, fit_rx, k = k, tm = tm, t0 = t0, rtn = "r")

  names(x0) <- names(param_r)[1]

  try(r[r<min_r] <- min_r)
  try(r[r>=max_r] <- max_r)

  param_rxt <- list(r=r,
                    x0=x0,
                    t0=as.POSIXct(names(param_r)[1],
                                  tz = "UTC"))
  return(param_rxt)
}


#' Fit non-linear model to raster layers
#'
#' @param xyi Spatrast stack
#' @param k carrying capacity, which is the maximum LAI that can be attained.
#'  This value can be cultivar-dependent. The default is fixed to 6
#' @param tm numeric, form of dates from each raster layer in `xyi`
#' @param t0 numeric, form of date from first raster layer in `xyi`
#' @param rtn character, either "x0" for the fitted value of the first layer or
#'  "r" for the estimated value.
#'
#' @return SpatRast, see `rtn`
#' @noRd
fit_rx <- function(xyi, k, tm,t0,rtn){
  if(any(is.na(xyi))) return(NA_real_)
  dataij <- data.frame(Tm = tm, XYi = xyi)
  fitted_rx <-
    minpack.lm::nlsLM(XYi ~ k/(1 + ((k-x_ij)/x_ij)*exp(-r_ij*(Tm-t0))),
                      start = list(x_ij = 1,
                                   r_ij = 0.025),
                      lower = c(1e-6,
                                1e-6),
                      algorithm = "port",
                      data = dataij)
  if(rtn == "x0") return(fitted_rx$m$getAllPars()[1])
  if(rtn == "r") return(fitted_rx$m$getAllPars()[2])else(
     stop("rtn must be 'x0' or 'r'")
   )

}
