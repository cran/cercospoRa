# cercospoRa 0.0.1


## minor improvements
  - moved format_weather function into `cercospoRa` package to avoid `epiphytoolR`
  suggests problems with CRAN.  
  - Moved some _./ext_ data to internal data for testing only to reduce package 
  size.  
  - weathr data attribution updated  

# cercospoRa 0.0.0.9003
## New Features and minor improvements
  - Four functions have been added to deal with spatial heterogeneity:
    - `read_sb_growth_parameter`: read in and format raster images to allow 
     further computation.
    - `calc_r_x0`: fit logistic growth curve to the date.
    - `calc_c_closure`: to deduct canopy closure dates from the fitted curves.
    - `calc_epidemic_onset_from_image`: builds on the existing `calc_epidemic_onset` 
    function to compute a value for each pixel.
  - Example datasets have been provided to check the functions. 
  It consists of a folder containing two LAI maps computed from UAV images.  

## Features removed  
  - Removed methods from Racca paper and set them to a new branch 
  [plus_racca](https://github.com/PaulMelloy/cercospoRa/tree/plus_racca)

## Documentation fixes  
  - add metadata  
  - Further simplification of `main` and `dev` branches to only the Wolf and 
  Verreet functions.  
  - add badge  
  - spelling and grammar fixes
  

# cercospoRa 0.0.0.9001
 - Change package name  
 - Add pkgdown site  
 - Reduce functions to only those described by Wolf and Verreet (2005)  

# cercosporaR 0.0.0.9000
