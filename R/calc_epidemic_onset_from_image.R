#' Calculate epidemic onset from image
#'
#' @param start POSIXct, start date in which to begin calculating the epidemic
#'  onset, if not specified, the first date in the weather data will be used.
#' @param end POSIXct, end date, last date to complete calculating the epidemic
#'  onset, if not specified, the last date in the weather data will be used.
#' @param c_closure map of canopy closure dates formatted as number of days since
#' 1970-01-01. It defines the date to start running the model (Wolf)
#' @param weather data.table, formatted with \code{\link{format_weather}}
#' @param cultivar_sus character, susceptibility of the cultivar in "R" resistant,
#'  "S" susceptible, "MR" moderately resistant etc.
#' @return If the input weather is conducive for epidemic, the function returns a
#'  POSIX_ct date when epidemic commences. If no epidemic occurs, a numeric,
#'  proportion indicating the progress an epidemic is returned
#'
#' @export
#'
#' @examples
#' wethr <- read.csv(system.file("extdata", "clean_weather.csv",
#'                   package = "cercospoRa"))
#' wethr <- format_weather(wethr,time_zone = "UTC")
#'
#' img_dir <- system.file("extdata", "uav_img",package = "cercospoRa")
#'
#' epidemic_onset_param <-
#'    read_sb_growth_parameter(
#'       list.files(img_dir,pattern = "tif",
#'                  full.names = TRUE),
#'       img_dates = as.POSIXct(c("2022-06-14","2022-06-28"),
#'                              tz = "UTC"),
#'       10)
#' param_rxt <- calc_r_x0(epidemic_onset_param,
#'                        min_r = 0.02,
#'                        max_r = 0.05,
#'                        k = 6)
#' c_closure <- calc_c_closure(param_rxt,
#'                             x1 = 1.3,
#'                             k=6 )
#'\donttest{ # this takes about 20 sec to run
#' epidemic_onset_map <- calc_epidemic_onset_from_image(start = as.POSIXct("2022-04-25",tz = "UTC"),
#'                                                      end = as.POSIXct("2022-09-30",tz = "UTC"),
#'                                                      c_closure = c_closure,
#'                                                      weather = wethr)
#'
#' terra::plot(epidemic_onset_map)
#' }
calc_epidemic_onset_from_image <- function(start,
                                           end,
                                           c_closure,
                                           weather,
                                           cultivar_sus = 5){
  # initialise onset raster

  Ep_onset <- terra::app(x = c_closure,
                        calc_r_onset,
                        start = start,
                        end = end,
                        weather = weather,
                        cultivar_sus = cultivar_sus)

  return(Ep_onset)
}

#' Calculate epidemic onset from raster
#'
#' @details
#'  Wrapper function to help internally calculate the earliest CLS onset date
#'  from canopy closure dates supplied as integers with an origin date of 1970-01-01
#'
#' @param r SpatRaster with integer onset dates as values. This is usually the
#'  output of `calc_c_closure`
#' @param ... additional arguments to be supplied to `calc_epidemic_onset()`
#'
#' @return SpatRaster with integer values representing days since origin "1970-01-01"
#' @noRd
calc_r_onset <- function(r, ...){
  #cat(r," | ")
  c_closure_date <- as.Date.numeric(round(r),
                                    origin = '1970-01-01')
  onset_epidemic_date <- calc_epidemic_onset(c_closure = c_closure_date, ...)
  return(as.integer(onset_epidemic_date))
}

