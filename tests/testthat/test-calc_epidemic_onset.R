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

  wethr <- read.csv(system.file("extdata", "clean_weather.csv",
                    package = "cercospoRa"))
  wethr <- format_weather(wethr,time_zone = "UTC")


  # susceptible cultivar
  sus_out <- calc_epidemic_onset(c_closure = as.POSIXct("2022-06-01", tz = "UTC"),
                      weather = wethr,
                      cultivar_sus = 6)
  expect_type(sus_out,"double")
  expect_equal(sus_out, as.POSIXct("2022-06-24",tz = "UTC"))

  # resistant cultivar
  res_out <- calc_epidemic_onset(c_closure = as.POSIXct("2022-06-01", tz = "UTC"),
                      weather = wethr,
                      cultivar_sus = 4)
  expect_type(res_out,"double")
  expect_equal(res_out, as.POSIXct("2022-07-13",tz = "UTC"),tolerance = 0.0000001)

  expect_true(res_out > sus_out)

  # resistant cultivar
  expect_warning(calc_epidemic_onset(c_closure = as.POSIXct("2022-06-01", tz = "UTC"),
                                 weather = wethr[times < as.POSIXct("2022-07-01", tz = "UTC")],
                                 cultivar_sus = 1))

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
                               cultivar_sus = 5)

    out2 <- c(out2,as.character(out))

  }

  expect_equal(out2, c('2022-06-09', '2022-06-10', '2022-06-12', '2022-06-14',
                       '2022-06-16', '2022-06-20', '2022-06-25', '2022-06-27',
                       '2022-06-28', '2022-06-29', '2022-06-30', '2022-07-02',
                       '2022-07-07', '2022-07-09', '2022-07-11', '2022-07-11',
                       '2022-07-14', '2022-07-18', '2022-07-22', '2022-07-25',
                       '2022-07-30', '2022-07-30', '2022-08-01', '2022-08-05',
                       '2022-08-09', '2022-08-11', '2022-08-14', '2022-08-18',
                       '2022-08-19', '2022-08-20'))

})

test_that("susceptibility calculator",{

  expect_equal(calc_susceptibility(4),12.4724)

  expect_equal(calc_susceptibility(3),17.1155)
  expect_equal(calc_susceptibility(5),7.8293)

  expect_equal(calc_susceptibility(2),19.038735)
  expect_equal(calc_susceptibility(6),5.906065)

  expect_equal(calc_susceptibility(1),20.51449, tolerance = 0.00001)
  expect_equal(calc_susceptibility(7),4.430315, tolerance = 0.00001)

  expect_equal(calc_susceptibility(8),3.1862, tolerance = 0.00001)
  expect_equal(calc_susceptibility(9),2.090113, tolerance = 0.00001)

  # check it errors
  expect_error(calc_susceptibility(10))
  expect_error(calc_susceptibility(0))

  # test it converts real numbers
  expect_equal(calc_susceptibility(4.235), 10.22157, tolerance = 0.00001)

  # plot(y = sapply(seq(1,9,0.02), calc_susceptibility),
  #      x = seq(1,9,0.02),
  #      ylim = c(0,20),ylab = "cDIV",
  #      xlab = "Susceptibility rating")

})
