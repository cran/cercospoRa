#' Calculate epidemic onset
#'
#' @param start POSIXct, start date in which to begin calculating the epidemic
#'  onset, if not specified, the first date in the weather data will be used.
#' @param end POSIXct, end date, last date to complete calculating the epidemic
#'  onset, if not specified, the last date in the weather data will be used.
#' @param c_closure POSIXct formatted date to start the model running the model
#'  This is usually at canopy closure (Wolf)
#' @param weather data.table, formatted with \code{\link{format_weather}}
#' @param cultivar_sus numeric, susceptibility of the cultivar on the 'bsa'
#'  https://www.bundessortenamt.de variety list.
#'  Susceptibility rating must range between 1 and 9.
#'  1 = resistant, 9 = susceptible. Default is 5.
#'
#' @return If the input weather is conducive for epidemic, the
#'  function returns a POSIX_ct date when epidemic commences. If no epidemic
#'  occurs, a numeric, proportion indicating the progress an epidemic is returned
#' @export
#'
#' @examples
#' wethr <- read.csv(system.file("extdata", "clean_weather.csv",
#'                   package = "cercospoRa"))
#' wethr <- format_weather(wethr,time_zone = "UTC")
#'
#' calc_epidemic_onset(start = as.POSIXct("2022-04-25",tz = "UTC"),
#'                     end = as.POSIXct("2022-09-30",tz = "UTC"),
#'                     c_closure = as.POSIXct("2022-07-01",tz = "UTC"),
#'                     weather = wethr)
calc_epidemic_onset <- function(start,
                                end,
                                c_closure,
                                weather,
                                cultivar_sus = 5){
  rh <- times <- DIV <- Year <- Month <- Day <- DIV_racca <- NULL
  if(inherits(weather,"epiphy.weather") == FALSE){
    stop("'weather' has not been formatted with 'format_weather().")
  }
  if(missing(start)) start <- as.Date(weather$times[1])
  if(missing(end)) end <- as.Date(last(weather$times))
  if(missing(c_closure)){
    warning("'c_closure' not supplied, setting 'start' as canopy closure date")
    c_closure <- start
  }

  c_closure <- as.POSIXct(c_closure,tz = "UTC")

  out <- sapply(c_closure,function(cc){
    if(is.na(cc)) return(NA)
    if(cc >= end) stop("'c_closure' is after last weather date")

    w <- copy(weather[times >= as.POSIXct(cc) &
                        times < (as.POSIXct(end) + 3600),])

    daily_inf_val <- calc_DIV(dat = w)

    div_cs <- daily_inf_val[first(which(cumsum(DIV) > calc_susceptibility(cultivar_sus))),
                            as.POSIXct(paste(Year,Month,Day,sep = "-"), tz = "UTC")]


    if(length(div_cs) == 0) {
      warning("weather conditions not suitable for epidemic onset within timeframe
      supplied.
            NA returned.")
      return(NA)}
    # calculate percentage

    return(div_cs[1])
  })

  return(as.POSIXct(out,tz = "UTC"))
}
