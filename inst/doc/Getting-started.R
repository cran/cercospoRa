## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(cercospoRa)
library(data.table)
library(terra)
library(sf)
library(ggplot2)

## ----prep_wdata---------------------------------------------------------------
# classify to data.table
wthr <- data.table(weathr)

# Use POSIXct formatted time.
wthr[,Time := as.POSIXct(paste0(Datum, " ",Stunde,":00"),tz = "UTC")]

# Nominate Latitude and Longitude location of the weather station. 
# While not needed in cercospoRa some plant disease models will use location to 
#  decide the closest weather station to pull weather from
wthr[, c("lon","lat") := list(9.916,51.41866)]

# weather is hourly and will error if we don't specify a standard deviation of 
#  weather direction. This is intentional to force the user to decide how variable
#  the wind direction data could be.
wthr[, wd_std := 20]

# remove all data after September as it contains missing data
wthr <- wthr[Datum < as.POSIXct("2022-10-01")]

# set NA wind speed values to zero
wthr[is.na(WG200), WG200 := 0]

## ----format_wdata-------------------------------------------------------------
wthr <- format_weather(wthr,
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
# As the data is formatted closely enough for what is expected for the model. 
# We can elect to turn the data_check off so 

## -----------------------------------------------------------------------------
cercospoRa::calc_epidemic_onset(start = as.POSIXct("2022-04-25",tz = "UTC"),
                    end = as.POSIXct("2022-09-30",tz = "UTC"),
                    c_closure = as.POSIXct("2022-07-30",tz = "UTC"),
                    weather = wthr,
                    cultivar_sus = 6)

## -----------------------------------------------------------------------------
# Get file location of example rasters with LAI values
image_files <- list.files(system.file("extdata", "uav_img",package = "cercospoRa"),
                          pattern = "tif",
                          full.names = TRUE)

# Read in data and check for consistency  
epidemic_onset_param <-
  read_sb_growth_parameter(img_files = image_files,
                           img_dates = as.POSIXct(c("2022-06-14","2022-06-28"),
                                                  tz = "UTC"),
                           target_res = 10)

terra::plot(epidemic_onset_param)

## -----------------------------------------------------------------------------
param_rxt <- calc_r_x0(epidemic_onset_param,
                      min_r = 0.02,
                      max_r = 0.05,
                      k = 6)

## -----------------------------------------------------------------------------
canopy_closure <- calc_c_closure(param_rxt,
                                 x1 = 1.3,
                                 k=6)
terra::plot(canopy_closure)

## -----------------------------------------------------------------------------
epidemic_onset_map <- 
  calc_epidemic_onset_from_image(start =as.POSIXct("2022-04-25",tz = "UTC"),
                                 end = as.POSIXct("2022-09-30",tz = "UTC"),
                                 cc_r = canopy_closure,
                                 weather = wthr,
                                 cultivar_sus = 6)
epidemic_onset_map

## -----------------------------------------------------------------------------
terra::plot(epidemic_onset_map)

## ----date_extraction----------------------------------------------------------
onset_dates <- as.POSIXct(terra::values(epidemic_onset_map),
                          tz = "UTC",
                          origin = "1970-01-01")

## ----rectrangle---------------------------------------------------------------
# convert to a data.table
dtdate <- data.table(pixel = seq_along(onset_dates),
                     ODates = as.IDate(onset_dates))

# Summarise the range of epidemic onset dates
summary(onset_dates[is.na(onset_dates)== FALSE])

# Plot dates on boxplot
dtdate[is.na(ODates) == FALSE,] |>
ggplot(aes(y=ODates))+
  geom_boxplot()

