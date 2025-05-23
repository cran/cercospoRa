#' Read LAI images at several time points
#'
#' @param img_files character vector providing the file paths of at least two
#'  georeferenced images for the study location. Dates of image capture need to
#'  be defined in \code{img_dates} respectively.
#' @param img_dates POSIXct vector of dates corresponding to the images supplied in
#'  \code{img_files} respectively.
#'  To prevent timezone issues use UTC timezone \code{tz = "UTC"}.
#' @param target_res desired spatial resolution. \code{target_res} should be
#'  equal to or larger than the actual resolution of the images expressed in
#'  meters.
#'
#' @return \CRANpkg{terra}::\code{SpatRast} with a layer for each input layer. Each layer
#'  is named according to the \code{img_dates}.
#'  The output \code{SpatRast} is suitable for input in \code{calc_r_x0()}
#'
#' @export
#'
#' @examples
#' epidemic_onset_param <-
#'   read_sb_growth_parameter(img_files = list.files(system.file("extdata", "uav_img",
#'                                                               package = "cercospoRa"),
#'                                                  pattern = ".tif",
#'                                                  full.names = TRUE),
#'                            img_dates = as.POSIXct(c("2022-06-14",
#'                                                     "2022-06-28"),
#'                                                   tz = "UTC"),
#'                            target_res = 10)
read_sb_growth_parameter <- function(img_files,
                                     img_dates,
                                     target_res){

  if(length(img_files) != length(img_dates)){
  stop("The lengths of 'img_files' and 'img_dates' must be the same length and order")}

  if(isFALSE(all(file.exists(img_files)))){
    stop("Some or all 'img_files' paths can't be resolved")
  }

  img_dates <- as.POSIXct(img_dates,
                          tryFormats = c("%Y-%m-%d", "%Y_%m_%d"),
                          tz = "UTC")

  t0 <- img_dates[1]
  img0 <- terra::rast(img_files[1])
  resolut <- mean(terra::res(img0))

  target_res0 <- target_res
  if(sf::st_is_longlat(img0)){
    target_res0 <- target_res0*(360/4e7)
  }

  if(resolut<target_res){
    aggreg_fact <- round(target_res0/resolut)
    if(aggreg_fact>=2){
      img0 <- terra::aggregate(img0, fact=aggreg_fact, fun='mean')
    }
  }


  imgs <- img0
  for(ti in img_files[-1]){
    imgi <- terra::rast(ti)
    resolut <- mean(terra::res(imgi))

    target_resi <- target_res
    if(sf::st_is_longlat(img0)){
      target_resi <- target_resi*(360/4e7)
    }

    if(resolut<target_resi){
      aggreg_fact <- round(target_resi/resolut)
      if(aggreg_fact>=2){
        imgi <- terra::aggregate(imgi, fact=aggreg_fact, fun='mean')
      }
    }

    if(!terra::same.crs(terra::crs(imgi),
                        terra::crs(img0))[1] |
       !identical(terra::res(imgi),
                  terra::res(img0))){
      imgi <- terra::project(imgi,
                            img0,
                            res = terra::res(img0))
    }

    if(!identical(terra::ext(imgi),
                  terra::ext(img0))) {
      imgi <- terra::resample(imgi, img0, method = "bilinear")
      }
    imgi <- terra::crop(imgi,terra::ext(img0))

    imgs <- c(imgs, imgi)
  }

  names(imgs) <- img_dates

  return(imgs)
}
