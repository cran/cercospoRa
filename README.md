# cercospoRa  
<!-- badges: start -->
<a href="https://paul.melloy.com.au/cercospoRa/"><img src="man/figures/logo.png" align="right" height="138" alt="cercospoRa website" /></a>
[![repo status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->
<br>

This R package, `cercospoRa`, takes steps to automate the epidemiological 
modelling of Cercospora leaf spot epidemics in _Cercospora beticola_ in sugar 
beet farms, available as an R package.
The package contains functions to use remotely sensed spatially explicit empirical
data to estimate leaf area index (LAI) when the crop canopy closes. 
Crop canopy closure is a key variable in modelling the estimating when growers 
will need to be vigilant and protect their crop with fungicides to prevent a 
yield limiting Cercospora leaf spot epidemic.  

`cercospoRa` uses functions described by Wolf and Verreet (2005) "Factors 
Affecting the Onset of Cercospora Leaf Spot Epidemics in Sugar Beet and 
Establishment of Disease-Monitoring Thresholds" _Phytopathology_  

<br>  

## Installation  

Install the package from this github repository  

```
remotes::install_github(repo = "PaulMelloy/cercospoRa")
```

<br>  

## Getting started  
### Format weather data  
```r
library(data.table)
library(cercospoRa)

# Inspect raw weather station data
head(cercospoRa::weathr)
```

`weathr` is a `data.table` containing weather data recorded at a sugar beet field 
trial observing the spread and severity of *C. beticola*.

```r
# make a copy of the data
wthr <- data.table(weathr)

# Format times to POSIXct time with UTC timezone
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
wthr[is.na(WG200),WG200 := 0]

# set NA wind direction values to 20 degrees. Wind is not important for this model
wthr[is.na(WR200),WR200 := 20]

# format_weather() formats weather data to 
#  hourly and checks for missing data or any issues that may cause downstream faults
#  in the model.
wthr <- 
  format_weather(wthr,
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
```
<br>  

### Calculate the proportional progress towards an epidemic  
```r
# susceptible cultivar
calc_epidemic_onset(c_closure = as.POSIXct("2022-07-01"),
                    weather = wthr,
                    cultivar_sus = 3)
# resistant cultivar                    
calc_epidemic_onset(c_closure = as.POSIXct("2022-07-01"),
                    weather = wthr,
                    cultivar_sus = 5)                    
                    
```
This returns a `POSIXct` date for the onset of an epidemic for the susceptible 
and more resistant cultivar.
If the input weather data does not provide a window where a epidemic onset date 
is met, the proportional progress towards an epidemic is returned.

`calc_epidemic_onset()` is a wrapper for `calc_DIV()` which returns a data.table 
detailing the daily contribution towards the "daily infection values" (Wolf and Verreet, 2005). 
For more detailed output of daily infection values call `calc_DIV()`

<br>  

### Calculate daily infection values  
```r
calc_DIV(dat = wthr)
```
This produces a `data.table` detailing the daily infection value for each day using
the method described in Wolf and Verreet (2005). 

**Note:** Missing humidity values do not prevent the model from running and these
days are assumed to not progress the model. The Racca and Jörg model returns `NA` 
values and the Wolf model returns `0`.  

<br>  

## Notes for contributors  
The `main` branch is the production branch and only provides functions to recreate
the model described in Wolf and Verreet (2005) as explained in the paper. 
The `main` branch is locked, please contribute to the `dev` branch.  
The `plus_racca` (development) branch also includes functions to recreate other 
*C. beticola* mechanistic models published by Racca and Jörg (2007) and auxiliary 
functions which might be helpful for future versions.  

<br>  

## References  
Wolf, P. F., & Verreet, J. A. (2005). Factors Affecting the Onset of Cercospora 
Leaf Spot Epidemics in Sugar Beet and Establishment of Disease-Monitoring 
Thresholds. *Phytopathology*, 95(3), 269-274.  

Racca, P., and Jörg, E. (2007). CERCBET 3 – a forecaster for epidemic development 
of *Cercospora beticola*. *EPPO Bulletin*, 37(2), 344-349.  
