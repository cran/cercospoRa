# identify lon lat from file ---------------------------------------------------
test_that("`format_weather()` is able to identify the correct lat and lon values
          from file", {
             set.seed(27)
             # create data.frame of station coordinates
             write.csv(data.frame(
                station = c("69061", "16096"),
                lon = c(134.2734, 135.7243),
                lat = c(-33.52662, -33.26625)
             ),
             file = file.path(tempdir(), "stat_coord.csv"))

             dat_minutes <- 10080 # equal to, 7 * 24 * 60

             weather_dt <- format_weather(
                w = weather_station_data,
                YYYY = "Local.Time.YYYY",
                MM = "Local.Time.MM",
                DD = "Local.Time.DD",
                hh = "Local.Time.HH24",
                mm = "Local.Time.MI",
                rain = "Precipitation.since.last.observation.in.mm",
                ws = "Wind.speed.in.km.h",
                wd = "Wind.direction.in.degrees.true",
                temp = "Temperature.in.Degrees.c",
                rh = "Relative.Humidity",
                station = "Station.Number",
                lonlat_file = file.path(tempdir(), "stat_coord.csv"),
                time_zone = "UTC"
             )

             expect_s3_class(weather_dt, "epiphy.weather")
             expect_equal(
                names(weather_dt),
                c(
                   "times",
                   "temp","rh",
                   "rain",
                   "ws",
                   "wd",
                   "wd_sd",
                   "lon",
                   "lat",
                   "station",
                   "YYYY",
                   "MM",
                   "DD",
                   "hh",
                   "mm"
                )
             )
             expect_equal(dim(weather_dt), c(168, 15))
             expect_true(anyNA(weather_dt$lon) == FALSE)
             expect_true(anyNA(weather_dt$lat) == FALSE)
             expect_equal(unique(weather_dt$lon), 135.7243)
             expect_equal(unique(weather_dt$lat), -33.26625)
             expect_s3_class(weather_dt$times, "POSIXct")
             expect_equal(attributes(weather_dt$times)$tzone, "UTC")
             expect_equal(as.character(min(weather_dt$times)),
                          "2020-06-10 01:00:00")
             expect_equal(as.character(max(weather_dt$times)), "2020-06-17")
             expect_equal(round(min(weather_dt$rain), 0), 7)
             expect_equal(round(max(weather_dt$rain), 1), 12.4)
             expect_equal(round(min(weather_dt$ws), 1), 6.5)
             expect_equal(round(max(weather_dt$ws), 2), 10.99, tolerance = 0.00001)
             expect_equal(round(min(weather_dt$wd), 0), 1)
             expect_equal(round(max(weather_dt$wd), 1), 358.3)
             expect_equal(round(min(weather_dt$wd_sd), 0), 82)
             expect_equal(round(max(weather_dt$wd_sd), 0), 195)
             expect_equal(mean(weather_dt$YYYY), 2020)
             expect_equal(mean(weather_dt$MM), 6)
             expect_equal(min(weather_dt$DD), 10)
             expect_equal(max(weather_dt$DD), 16)
             expect_equal(min(weather_dt$hh), 1)
             expect_equal(max(weather_dt$hh), 24)
             expect_equal(mean(weather_dt$mm), 0)
             expect_equal(lubridate::year(weather_dt[,times]), weather_dt[,YYYY])
             expect_equal(lubridate::month(weather_dt[,times]), weather_dt[,MM])
             # expect_equal(lubridate::day(weather_dt[,times]), weather_dt[,DD])
             # expect_equal(lubridate::hour(weather_dt[,times]), weather_dt[,hh])
             expect_equal(lubridate::minute(weather_dt[,times]), weather_dt[,mm])

             unlink(file.path(tempdir(), "stat_coord.csv"))
          })

# when more than one station is supplied, lapply is used -----------------------

# test_that("`format_weather()` handles multiple stations", {
#    scaddan <-
#       system.file("extdata", "scaddan_weather.csv", package = "cercospoRa")
#
#    weather_file_list <- list(scaddan, naddacs)
#    weather_station_data <-
#       list(read.csv(scaddan),naddacs)
#
#    weather_station_data <- do.call("rbind", weather_station_data)
#
#    weather_station_data$Local.Time <-
#       as.POSIXct(weather_station_data$Local.Time, format = "%Y-%m-%d %H:%M:%S")
#
#
#    # a missing time at "2020-10-03 16:00:00" in scaddan data causes NAs to be
#    #  filled into all data inputs
#    # this creates a error later due to .check_weather()
#    expect_error(suppressWarnings(
#       format_weather(
#          w = weather_station_data[c(1:4123,4125:8785),], # remove one row
#          POSIXct_time = "Local.Time",
#          ws = "meanWindSpeeds",
#          wd_sd = "stdDevWindDirections",
#          rain = "Rainfall",
#          temp = "Temperature",
#          wd = "meanWindDirections",
#          lon = "Station.Longitude",
#          lat = "Station.Latitude",
#          station = "StationID",
#          time_zone = "Australia/Brisbane"
#       )
#    ), regexp = "Time records contain NA values or duplicated times *")
#
#    expect_error(expect_warning(
#    weather_dat <- format_weather(
#       w = weather_station_data[c(1:4123,4125:8785),],
#       POSIXct_time = "Local.Time",
#       ws = "meanWindSpeeds",
#       wd_sd = "stdDevWindDirections",
#       rain = "Rainfall",
#       wd = "meanWindDirections",
#       lon = "Station.Longitude",
#       lat = "Station.Latitude",
#       station = "StationID",
#       time_zone = "Australia/Adelaide",
#       data_check = FALSE
#       )))
#
#    expect_s3_class(weather_dat, "epiphy.weather")
#    expect_equal(
#       names(weather_dat),
#       c(
#          "times",
#          "temp",
#          "rh",
#          "rain",
#          "ws",
#          "wd",
#          "wd_sd",
#          "lon",
#          "lat",
#          "station",
#          "YYYY",
#          "MM",
#          "DD",
#          "hh",
#          "mm"
#       )
#    )
#    expect_equal(dim(weather_dat), c(8786, 15))
#    expect_true(anyNA(weather_dat$lon) == FALSE)
#    expect_true(anyNA(weather_dat$lat) == FALSE)
#    expect_equal(round(unique(weather_dat$lon), 1), c(135.9,135.7))
#    expect_equal(round(unique(weather_dat$lat), 1), c(-33.3,-33.1))
# })

# identify lon lat from cols ---------------------------------------------------
test_that("`format_weather()` works when lat lon are in data", {
   dat_minutes <- 10080 # equal to, 7 * 24 * 60

   weather_station_data <- data.table(
      Local.Time.YYYY = rep(2020, dat_minutes),
      Local.Time.MM = rep(6, dat_minutes),
      Local.Time.DD = rep(10:16, each = 24 * 60),
      Local.Time.HH24 = rep(1:24, each = 60, times = 7),
      Local.Time.MI = rep(0:59, times = 7 * 24),
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         dat_minutes, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(
         dat_minutes, mean = 5, sd = 10
      )),
      Wind.direction.in.degrees.true =
         runif(n = dat_minutes, min = 0, max = 359),
      Station.Number = "16096",
      Temperature.in.Degrees.c = rnorm(10080,15,3),
      lon = 135.7243,
      lat = -33.26625
   )

   weather_dt <- format_weather(
      w = weather_station_data,
      YYYY = "Local.Time.YYYY",
      MM = "Local.Time.MM",
      DD = "Local.Time.DD",
      hh = "Local.Time.HH24",
      mm = "Local.Time.MI",
      rain = "Precipitation.since.last.observation.in.mm",
      temp = "Temperature.in.Degrees.c",
      ws = "Wind.speed.in.km.h",
      wd = "Wind.direction.in.degrees.true",
      station = "Station.Number",
      lon = "lon",
      lat = "lat",
      time_zone = "UTC",
      data_check = c("temp","rain","ws","wd")
   )

   expect_s3_class(weather_dt, "epiphy.weather")
   expect_named(
      weather_dt,
      c(
         "times",
         "temp",
         "rh",
         "rain",
         "ws",
         "wd",
         "wd_sd",
         "lon",
         "lat",
         "station",
         "YYYY",
         "MM",
         "DD",
         "hh",
         "mm"
      )
   )
   expect_equal(dim(weather_dt), c(168, 15))
   expect_s3_class(weather_dt$times, "POSIXct")
   expect_true(anyNA(weather_dt$times) == FALSE)
   expect_true(max(weather_dt$wd, na.rm = TRUE) < 360)
   expect_true(min(weather_dt$wd, na.rm = TRUE) > 0)
   expect_true(lubridate::tz(weather_dt$times) == "UTC")
})

# stop if `x` is not a data.frame object ---------------------------------------
test_that("`format_weather()` stops if `x` is not a data.frame object", {
   expect_error(
      weather_dt <- format_weather(
         w = list(),
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         lon = "lon",
         lat = "lat",
         POSIXct_time = "times"
      ),
      regexp = "`w` must be provided as a `data.frame` object*"
   )
})

# stop if time isn't given in any col ------------------------------------------
test_that("`format_weather()` stops if time cols are not provided", {
   dat_minutes <- 10080 # equal to, 7 * 24 * 60

   weather_station_data <- data.table(
      Local.Time.YYYY = rep(2020, dat_minutes),
      Local.Time.MM = rep(6, dat_minutes),
      Local.Time.DD = rep(10:16, each = 24 * 60),
      Local.Time.HH24 = rep(1:24, each = 60, times = 7),
      Local.Time.MI = rep(0:59, times = 7 * 24),
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         dat_minutes, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(
         dat_minutes, mean = 5, sd = 10
      )),
      Wind.direction.in.degrees.true =
         runif(n = dat_minutes, min = 0, max = 359),
      Temperature = rnorm(10080, 25, 5),
      Station.Number = "16096",
      lon = 135.7243,
      lat = -33.26625
   )

   weather_dt <- format_weather(
      w = weather_station_data,
      YYYY = "Local.Time.YYYY",
      MM = "Local.Time.MM",
      DD = "Local.Time.DD",
      hh = "Local.Time.HH24",
      mm = "Local.Time.MI",
      rain = "Precipitation.since.last.observation.in.mm",
      temp = "Temperature",
      ws = "Wind.speed.in.km.h",
      wd = "Wind.direction.in.degrees.true",
      station = "Station.Number",
      lon = "lon",
      lat = "lat",
      time_zone = "UTC",
      data_check = c("temp","rain","ws","wd")
      )

   expect_error(
      weather_dt <- format_weather(
         w = weather_station_data,
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         lon = "lon",
         lat = "lat"
      ),
      regexp = "You must provide time values either as a*"
   )
})

# stop if lonlat file lacks proper field names ---------------------------------
test_that("`format_weather() stops if lonlat input lacks proper names", {
   # create a dummy .csv with misnamed cols
   write.csv(data.frame(
      stats = c("69061", "16096"),
      long = c(134.2734, 135.7243),
      lat = c(-33.52662, -33.26625)
   ),
   file = file.path(tempdir(), "stat_coord.csv"))

   dat_minutes <- 10080 # equal to, 7 * 24 * 60


   weather_station_data <- data.table(
      Local.Time.YYYY = rep(2020, dat_minutes),
      Local.Time.MM = rep(6, dat_minutes),
      Local.Time.DD = rep(10:16, each = 24 * 60),
      Local.Time.HH24 = rep(1:24, each = 60, times = 7),
      Local.Time.MI = rep(0:59, times = 7 * 24),
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         dat_minutes, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(
         dat_minutes, mean = 5, sd = 10
      )),
      Wind.direction.in.degrees.true =
         runif(n = dat_minutes, min = 0, max = 359),
      Station.Number = "16096",
      lon = 135.7243,
      lat = -33.26625
   )

   expect_error(
      format_weather(
         w = weather_station_data,
         YYYY = "Local.Time.YYYY",
         MM = "Local.Time.MM",
         DD = "Local.Time.DD",
         hh = "Local.Time.HH24",
         mm = "Local.Time.MI",
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         lonlat_file = file.path(tempdir(), "stat_coord.csv"),
         time_zone = "UTC"
      ), regexp = "The CSV file of weather station coordinates should contain column names *"
   )

   unlink(file.path(tempdir(), "stat_coord.csv"))
})

# stop if no lonlat info provided ----------------------------------------------
test_that("`format_weather() stops if lonlat input lacks proper names", {
   dat_minutes <- 10080 # equal to, 7 * 24 * 60

   weather_station_data <- data.table(
      Local.Time.YYYY = rep(2020, dat_minutes),
      Local.Time.MM = rep(6, dat_minutes),
      Local.Time.DD = rep(10:16, each = 24 * 60),
      Local.Time.HH24 = rep(1:24, each = 60, times = 7),
      Local.Time.MI = rep(0:59, times = 7 * 24),
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         dat_minutes, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(
         dat_minutes, mean = 5, sd = 10
      )),
      Wind.direction.in.degrees.true =
         runif(n = dat_minutes, min = 0, max = 359),
      Station.Number = "16096",
      lon = 135.7243,
      lat = -33.26625
   )

   expect_error(
      weather_dt <- format_weather(
         w = weather_station_data,
         YYYY = "Local.Time.YYYY",
         MM = "Local.Time.MM",
         DD = "Local.Time.DD",
         hh = "Local.Time.HH24",
         mm = "Local.Time.MI",
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         time_zone = "UTC"
      ),
      regexp = "You must provide lonlat values for the weather *"
   )
   unlink(file.path(tempdir(), "stat_coord.csv"))
})


# fill missing mm --------------------------------------------------------------
test_that("`format_weather() creates a `mm` column if not provided", {
   dat_minutes <- 10080 # equal to, 7 * 24 * 60

   time1 <- seq(from = as.POSIXct("2020-06-10 00:00:00"),
                to = as.POSIXct("2020-06-16 23:59:59"),
                length.out = 10080)

   weather_station_data <- data.table(
      Local.Time.YYYY = year(time1),
      Local.Time.MM = month(time1),
      Local.Time.DD = as.numeric(format(time1, format = "%d")),
      Local.Time.HH24 = hour(time1),
      Local.Time.mm = minute(time1),
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         dat_minutes, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(
         dat_minutes, mean = 5, sd = 10
      )),
      Temperature = rnorm(10080, cos((1:25+18)/3.8)*11+25, sd = 2),
      Wind.direction.in.degrees.true =
         runif(n = dat_minutes, min = 0, max = 359),
      Station.Number = "16096",
      lon = 135.7243,
      lat = -33.26625
   )

   expect_named(
      weather_dt <- format_weather(
         w = weather_station_data,
         YYYY = "Local.Time.YYYY",
         MM = "Local.Time.MM",
         DD = "Local.Time.DD",
         hh = "Local.Time.HH24",
         mm = "Local.Time.mm",
         rain = "Precipitation.since.last.observation.in.mm",
         temp = "Temperature",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         lat = "lat",
         lon = "lon",
         time_zone = "UTC",
         data_check = c("temp","rain","ws","wd")
      ),
      c(
         "times",
         "temp",
         "rh",
         "rain",
         "ws",
         "wd",
         "wd_sd",
         "lon",
         "lat",
         "station",
         "YYYY",
         "MM",
         "DD",
         "hh",
         "mm"
      )
   )
})

# fill create YYYY, MM, DD hhmm cols from POSIXct ------------------------------
test_that("`format_weather() creates a YYYY MM DD... cols", {
   dat_minutes <- 10080 # equal to, 7 * 24 * 60

   weather_station_data <- data.table(
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         dat_minutes, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(
         dat_minutes, mean = 5, sd = 10
      )),
      Wind.direction.in.degrees.true =
         runif(n = dat_minutes, min = 0, max = 359),
      Station.Number = "16096",
      Ptime = seq(ISOdate(2000, 1, 1), by = "1 min", length.out = dat_minutes),
      lon = 135.7243,
      lat = -33.26625,
      time_zone = c("Australia/Adelaide"),
      Temperature = rnorm(dat_minutes,20,10)
   )

   expect_warning(
   expect_named(
      format_weather(
         w = weather_station_data,
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         temp = "Temperature",
         station = "Station.Number",
         lat = "lat",
         lon = "lon",
         POSIXct_time = "Ptime",
         time_zone = "Australia/Brisbane",
         data_check = c("temp","rain","ws","wd")
      ),
      c(
         "times",
         "temp",
         "rh",
         "rain",
         "ws",
         "wd",
         "wd_sd",
         "lon",
         "lat",
         "station",
         "YYYY",
         "MM",
         "DD",
         "hh",
         "mm"
      )
   ))
})

# stop if `wd_sd` is missing or cannot be calculated ---------------------------
test_that("`format_weather() stops if `wd_sd` is not available", {
   weather_station_data <- data.table(
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         24, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(24, mean = 5, sd = 10)),
      Wind.direction.in.degrees.true =
         runif(n = 24, min = 0, max = 359),
      Station.Number = "16096",
      Ptime = seq(ISOdate(2000, 1, 1), by = "1 hour", length.out = 24),
      lon = 135.7243,
      lat = -33.26625
   )

   expect_error(
      format_weather(
         w = weather_station_data,
         YYYY = "Local.Time.YYYY",
         MM = "Local.Time.MM",
         DD = "Local.Time.DD",
         hh = "Local.Time.HH24",
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         lat = "lat",
         lon = "lon",
         POSIXct_time = "Ptime",
         time_zone = "Australia/Brisbane"
      ),
      regexp = "* was unable to detect or calculate `wd_sd`.*"
   )
})

# stop if no raster, `r` or `time_zone` provided -------------------------------
test_that("`format_weather() stops if `time_zone` cannot be determined", {
   weather_station_data <- data.table(
      Precipitation.since.last.observation.in.mm = round(abs(rnorm(
         24, mean = 0, sd = 0.2
      )), 1),
      Wind.speed.in.km.h = abs(rnorm(24, mean = 5, sd = 10)),
      Wind.direction.in.degrees.true =
         runif(n = 24, min = 0, max = 359),
      Station.Number = "16096",
      Ptime = seq(ISOdate(2000, 1, 1), by = "1 hour", length.out = 24),
      lon = 135.7243,
      lat = -33.26625
   )

   expect_error(
      format_weather(
         w = weather_station_data,
         YYYY = "Local.Time.YYYY",
         MM = "Local.Time.MM",
         DD = "Local.Time.DD",
         hh = "Local.Time.HH24",
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         lat = "lat",
         lon = "lon",
         POSIXct_time = "Ptime"
      ),
      regexp =  "Please include the `time_zone*"
   )
})


test_that("format_weather detects impossible times", {
   raw_weather <- data.table(
      Year = rep(2020, 14 * 24 * 60),
      Month = rep(6, 14 * 24 * 60),
      Day = rep(rep(1:7, each = 24 * 60), 2),
      Hour = rep(rep(0:23, each = 60), 14),
      Minute = rep(1:60, 14 * 24),
      WindSpeed = abs(rnorm(14 * 24 * 60, 1, 3)),
      WindDirectionDegrees = round(runif(14 * 24 * 60, 0, 359)),
      Rainfall = floor(abs(rnorm(14 * 24 * 60, 0, 1))),
      stationID = rep(c("12345", "54321"), each = 7 * 24 * 60),
      StationLongitude = rep(c(134.123, 136.312), each = 7 * 24 * 60),
      StationLatitude = rep(c(-32.321, -33.123), each = 7 * 24 * 60)
   )


   expect_warning(expect_error(
      format_weather(
         w = raw_weather,
         YYYY = "Year",
         MM = "Month",
         DD = "Day",
         hh = "Hour",
         mm = "Minute",
         ws = "WindSpeed",
         rain = "Rainfall",
         wd = "WindDirectionDegrees",
         lon = "StationLongitude",
         lat = "StationLatitude",
         station = "stationID",
         time_zone = "Australia/Darwin"
      ), regexp = "Time records contain NA values or duplicated times *"
   ))
})

set.seed(27)
# create data.frame of station coordinates
write.csv(data.frame(
   station = c("069061", "016096"),
   lon = c(134.2734, 135.7243),
   lat = c(-33.52662,-33.26625)
),
file = file.path(tempdir(), "stat_coord.csv"))

dat_minutes <- 60 * 24 * 365

weather_station_data <- data.table(
   Time = seq(from = as.POSIXct("2020-01-01 01:00:00", format = "%Y-%m-%d %H:%M:%S"),
              length.out = dat_minutes,
              by = "1 min"),
   Precipitation.since.last.observation.in.mm = round(abs(rnorm(
      dat_minutes, mean = 0, sd = 0.2
   )), 1),
   Wind.speed.in.km.h = abs(rnorm(
      dat_minutes, mean = 5, sd = 10
   )),
   Wind.direction.in.degrees.true = runif(n = dat_minutes,
                                          min = 0, max = 359),
   Station.Number = rep("016096", dat_minutes)
)

test10 <- format_weather(
   w = weather_station_data,
   POSIXct_time = "Time",
   rain = "Precipitation.since.last.observation.in.mm",
   ws = "Wind.speed.in.km.h",
   wd = "Wind.direction.in.degrees.true",
   station = "Station.Number",
   lonlat_file = file.path(tempdir(), "stat_coord.csv"),
   time_zone = "Australia/Sydney",
   data_check = FALSE
)


test_that("lat and lon are correctly parsed from file to dataset", {
   expect_false(test10[, any(is.na(lat))])
   expect_false(test10[, any(is.na(lon))])

   weather_station_data[, Station.Number := 016096]

   expect_error(
      test10.1 <- format_weather(
         w = weather_station_data,
         POSIXct_time = "Time",
         rain = "Precipitation.since.last.observation.in.mm",
         ws = "Wind.speed.in.km.h",
         wd = "Wind.direction.in.degrees.true",
         station = "Station.Number",
         lonlat_file = file.path(tempdir(), "stat_coord.csv"),
         time_zone = "Australia/Sydney"
      ),
      regexp = "'station' name '16096' cannot be found in latlon_file."
   )
})

test_that("preformated weather read back in can be reformatted",{
   # since R 4.3 write.csv saves midnight date-times as date without 00:00:00
   # therefore we need data.tables fwrite function
   tmp_fname <- paste(tempdir(), "weather_saved.csv", sep = "\\")
   write.csv(test10, file = tmp_fname,
             row.names = FALSE)
   weather2 <- read.csv(tmp_fname,
                        stringsAsFactors = FALSE)

   expect_warning(test11 <- format_weather(weather2, time_zone = "UTC",
                                           data_check = c("temp","rain","ws","wd")),
                  regexp = "All temperature values are 'NA' or missing*")
   expect_warning(format_weather(weather2, time_zone = "Australia/Perth",
                                 data_check = c("temp","rain","ws","wd")),
                     regexp = "All temperature values are 'NA' or missing*")


   expect_equal(class(test11), c("epiphy.weather", "data.table", "data.frame"))
   expect_equal(class(test11[,times]), c("POSIXct", "POSIXt"))
   expect_equal(dim(test11), c(8760,15))

   expect_warning(format_weather(weather2, time_zone = "Australia/Sydney",
                                 data_check = c("temp","rain","ws","wd")),
                     regexp = "All temperature values are 'NA' or missing*")
   unlink(tmp_fname)
})


test_that("`format_weather()` fills missing time", {
   scaddan <-
      system.file("extdata", "scaddan_weather.csv", package = "cercospoRa")

   weather_station_data <- read.csv(scaddan)

   weather_station_data$Local.Time <-
      as.POSIXct(weather_station_data$Local.Time, format = "%Y-%m-%d %H:%M:%S",tz = "Australia/Brisbane")

   # remove ten readings
   weather_station_data <-
      weather_station_data[-(10:19),]

  expect_warning(
   expect_error(
   expect_warning(
     weather_dat <- format_weather(
        w = weather_station_data,
        POSIXct_time = "Local.Time",
        ws = "meanWindSpeeds",
        wd_sd = "stdDevWindDirections",
        rain = "Rainfall",
        wd = "meanWindDirections",
        lon = "Station.Longitude",
        lat = "Station.Latitude",
        station = "StationID",
        time_zone = "Australia/Brisbane",
        data_check = c("temp","rain","ws","wd")),
     regexp = "All temperature values are 'NA' or missing*"
  ), regexp = "NA values in rainfall"))


})

# test_that("`format_weather()` works with blackspot vignette", {
#    scaddan_weather <-
#       read.csv(system.file("extdata", "scaddan_weather.csv", package = "cercospoRa"))
#
#    raw_weather <- rbind(naddacs,
#                         scaddan_weather)
#
#    # All 'rain' data must be entered with no missing data
#    # Replace NA values in rain with zeros
#    # raw_weather[is.na(raw_weather$Rainfall),]
#
#    # Format time into POSIX central time
#    raw_weather$Local.Time <-
#       lubridate::as_datetime(raw_weather$Local.Time)
#
#    weather <- format_weather(
#       w = raw_weather,
#       POSIXct_time = "Local.Time",
#       ws = "meanWindSpeeds",
#       wd_sd = "stdDevWindDirections",
#       rain = "Rainfall",
#       temp = "Temperature",
#       wd = "meanWindDirections",
#       lon = "Station.Longitude",
#       lat = "Station.Latitude",
#       station = "StationID",
#       time_zone = "UTC",
#       data_check = c("temp","rain","ws","wd")
#    )
#    expect_equal(dim(weather), c(8786,15))
#
# })


test_that("Non-unique stations and coordinates are detected",{

   # This function causes a warning due to non-continuous weather data
   # data_check is set to false to
   expect_warning(
      out <-
      format_weather(w = b_wther,
                  POSIXct_time = "aifstime_utc",
                  temp = "air_temp",
                  rain = "rain_ten",
                  rh = "rel_hum",
                  ws = "wind_spd_kmh",
                  wd = "wind_dir_deg",
                  lon = "lon",
                  lat = "lat",
                  station = "name",
                  time_zone = "UTC",
                  data_check = FALSE))

   expect_false(any(duplicated(out$times)))
})
