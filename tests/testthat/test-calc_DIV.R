date_t <- unique(as.POSIXct(weathr$Datum))[c(1,2)]
Tm1 <- weathr$T200[c(1,288)]
RH1 <- weathr$F200[c(1,288)]
rain1 <- c(0,5)

test_that("calc_DIV works", {
  # individual vectors
  DIV1 <- calc_DIV(
    date_time = date_t,
    Tm = Tm1,
    RH = RH1,
    rain = rain1
  )
  expect_equal(DIV1$DIV, c(0.4373111, 0.0214606), tolerance = 0.00001)
  expect_type(DIV1,"list")
  expect_equal(dim(DIV1), c(2,4))
  expect_equal(colnames(DIV1), c("Year","Month","Day","DIV"))

  in_dat <- data.frame(
    date_time = date_t,
    temp = Tm1,
    RH = RH1,
    rain = rain1
  )

  expect_error(calc_DIV(dat = in_dat),
               regexp = "'dat' data.frame must have colnames 'times','temp','rh','rain'")

  # accept a data.frame as input
  in_dat <- data.frame(
    times = date_t,
    temp = Tm1,
    rh = RH1,
    rain = rain1
  )
  dat_out <- calc_DIV(dat = in_dat)

  expect_equal(dat_out$DIV, c(0.4373111, 0.0214606), tolerance = 0.00001)
  expect_type(dat_out,"list")
  expect_equal(dim(dat_out), c(2,4))
  expect_equal(colnames(dat_out), c("Year","Month","Day","DIV"))

})
