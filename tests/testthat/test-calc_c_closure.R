img_dir <- system.file("extdata", "uav_img",package = "cercospoRa")
epidemic_onset_param <-
  read_sb_growth_parameter(img_files = list.files(img_dir,pattern = "tif",
                                                  full.names = TRUE),
                           img_dates = as.POSIXct(
                             c("2022-06-14","2022-06-28"),tz = "UTC"),
                           target_res = 10)


param_rxt <- calc_r_x0(epidemic_onset_param,
                       min_r = 0.02,
                       max_r = 0.05,
                       k = 6)

test_that("growth rate is calculated correctly",{

  expect_no_condition(calc_r_x0(epidemic_onset_param,
                                min_r = 0.02,
                                max_r = 0.05,
                                k = 6))

  # output a named list with expected dates and dimensions
  expect_type(param_rxt,"list")
  expect_named(param_rxt,c("r","x0","t0"))

  expect_s4_class(param_rxt$r,"SpatRaster")
  expect_equal(dim(param_rxt$r), c(17,29,1))
  expect_equal(terra::minmax(param_rxt$r),
               matrix(c(0.02, 0.05),dimnames = list(c("min", "max"), "lyr.1")))

  expect_s4_class(param_rxt$x0,"SpatRaster")
  expect_equal(dim(param_rxt$x0), c(17,29,1))
  expect_equal(terra::minmax(param_rxt$x0),
               matrix(c(0.5216454, 2.9568495),dimnames = list(c("min", "max"), "2022-06-14")))

  expect_equal(param_rxt$t0, as.POSIXct("2022-06-14",tz = "UTC"))

})

test_that("canopy closure is calculated correctly",{

  cc <- calc_c_closure(param_rxt,
                              x1 = 1.3,
                              k = 6)

  expect_s4_class(cc,"SpatRaster")
  expect_equal(dim(cc), c(17,29,1))
  expect_equal(terra::minmax(cc),
               matrix(round(c(19093.5833, 19192.5833)),dimnames = list(c("min", "max"), "2022-06-14")))


})
