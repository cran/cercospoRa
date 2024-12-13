# load the weather data to be formatted
# import BOM data file
set.seed(321)
# Remove NAs
brisvegas[is.na(wind_dir_deg), wind_dir_deg := 90]

test_that("Relative humidity formats",{

  bris_formated <-
    format_weather(
      w = brisvegas,
      POSIXct_time = "aifstime_utc",
      time_zone = "UTC",
      temp = "air_temp",
      rh = "rel_hum",
      rain = "rain_trace",
      ws = "wind_spd_kmh",
      wd = "wind_dir_deg",
      station = "name",
      lon = "lon",
      lat = "lat",
      data_check = c("temp","rain")
   )

   # fill NAs with the same relative humidity as the previous day
   bris_formated[,rh := fifelse(is.na(rh),shift(rh,n=24,type = "lag"),
                               rh)]

   expect_false(any(is.na(bris_formated$rh)))
})

test_that("epidemic onset produces expected outcome", {
  bris_formated <- format_weather(
    w = brisvegas,
    POSIXct_time = "aifstime_utc",
    time_zone = "UTC",
    temp = "air_temp",
    rh = "rel_hum",
    rain = "rain_trace",
    ws = "wind_spd_kmh",
    wd = "wind_dir_deg",
    station = "name",
    lon = "lon",
    lat = "lat",
    data_check = c("temp","rain")
  )
  bris_formated[,rh := fifelse(is.na(rh),shift(rh,n=24,type = "lag"),
                               rh)]

  # susceptible cultivar
  sus_out <- calc_epidemic_onset(c_closure = as.POSIXct("2023-06-01", tz = "UTC"),
                      weather = bris_formated,
                      cultivar_sus = 3)
  expect_type(sus_out,"double")
  expect_equal(sus_out, as.POSIXct("2023-06-07",tz = "UTC"))

  # resistant cultivar
  res_out <- calc_epidemic_onset(c_closure = as.POSIXct("2023-06-01", tz = "UTC"),
                      weather = bris_formated,
                      cultivar_sus = 5)
  expect_type(res_out,"double")
  expect_equal(res_out, as.POSIXct("2023-06-17",tz = "UTC"),tolerance = 0.0000001)

})

test_that("different start dates provide different epidemic dates",{
  w_dat <- data.table(weathr)
  # Use POSIXct formatted time.
  w_dat[,Time := as.POSIXct(paste0(Datum, " ",Stunde,":00"),tz = "UTC")]
  w_dat[, c("lon","lat") := list(9.916,51.41866)]
  # weather is hourly and will error if we don't specify a wd standard deviation
  w_dat[, wd_std := 20]
  # set NA wind direction values to 20 degrees. Wind is not important for this model
  w_dat[,WR200 := runif(.N,min = 0,359)]
  # remove all data after September as it contains missing data
  w_dat <- w_dat[Datum < as.POSIXct("2022-10-01",tz = "UTC")]
  # set NA wind speed values to zero
  w_dat[is.na(WG200),WG200 := 0]


  w_dat <- format_weather(w_dat,
                         POSIXct_time = "Time",
                         time_zone = "UTC",
                         temp = "T200",
                         rain = "N100",
                         rh = "F200",
                         wd = "WR200",
                         ws = "WG200",
                         station = "Station",
                         lon = "lon",
                         lat = "lat",
                         wd_sd = "wd_std",
                         data_check = FALSE # this stops the function from checking for faults
  )

  for(i in 1:30){
    if(i == 1){
      out2 <- vector(mode = "character")
    }
    out <- calc_epidemic_onset(start = as.POSIXct("2022-04-25",tz = "UTC"),
                               end = as.POSIXct("2022-09-30",tz = "UTC"),
                               c_closure = as.POSIXct("2022-05-01",tz = "UTC")+
                                 (i*3*86400), # 86400 is the number of seconds in a day
                               weather = w_dat,
                               cultivar_sus = 3)

    out2 <- c(out2,as.character(out))

  }

  expect_equal(out2, c("2022-05-20", "2022-05-21", "2022-05-22", "2022-05-24",
                       "2022-05-25", "2022-05-31", "2022-06-06", "2022-06-08",
                       "2022-06-09", "2022-06-11", "2022-06-12", "2022-06-17",
                       "2022-06-22", "2022-06-25", "2022-06-26", "2022-06-27",
                       "2022-06-30", "2022-07-01", "2022-07-07", "2022-07-09",
                       "2022-07-11", "2022-07-13", "2022-07-20", "2022-07-23",
                       "2022-07-26", "2022-07-26", "2022-07-29", "2022-08-01",
                       "2022-08-03", "2022-08-05"))
})

