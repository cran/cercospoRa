test_that("temperature_index() examples works", {
  expect_equal(temperature_index(18),0.880226, tolerance = 0.000001)
  expect_equal(temperature_index(10),0.2288871, tolerance = 0.000001)
  expect_equal(temperature_index(28),1, tolerance = 0.000001)
  expect_equal(temperature_index(-28),0, tolerance = 0.000001)
})

test_that("latent_period() examples work", {
  expect_equal(latent_period(25), 7.136435, tolerance = 0.000001)
  expect_equal(latent_period(0), 868.002, tolerance = 0.000001)
  expect_equal(latent_period(35), 7.00412, tolerance = 0.000001)
  expect_equal(latent_period(20:25), c(7.78513196898028,
                                       7.55327314739781,
                                       7.38988499733242,
                                       7.27474731398017,
                                       7.19361115984403,
                                       7.13643547837872), tolerance = 0.000001)

  expect_equal(latent_period(25, ref = "jarroudi"), 11.53403, tolerance = 0.000001)
  expect_equal(latent_period(0, ref = "jarroudi"), 0, tolerance = 0.000001)
  expect_equal(latent_period(35, ref = "jarroudi"), 7.639419, tolerance = 0.000001)
  expect_equal(latent_period(20:25,
                             ref = "jarroudi"),
               c(15.47988 ,
                 14.48855 ,
                 13.61656 ,
                 12.84357 ,
                 12.15362 ,
                 11.53403), tolerance = 0.000001)
  # Compare methods
  # plot(10:40,latent_period(10:40), type = "l")
  # lines(10:40,latent_period(10:40, ref = "jarroudi"), type = "l", col = "blue")

  })

test_that("moisture_index(method =1) examples work", {
  expect_equal(moisture_index(25), 0)
  expect_equal(moisture_index(90), 1)
  expect_equal(moisture_index(95), 1)
  expect_equal(moisture_index(25, rain = 0.1), 1, tolerance = 0.0000000001)
  expect_equal(moisture_index(55,rh_thresh = 50), 1, tolerance = 0.00001)
})

test_that("moisture_index(method =2) examples work", {
  expect_equal(moisture_index(25, method = 2), 0)
  expect_equal(moisture_index(90, method = 2), 0.68879132)
  expect_equal(moisture_index(95, method = 2), 0.9416218)
  expect_equal(moisture_index(25, rain = 0.1, method = 2), 0, tolerance = 0.0000000001)
  expect_equal(moisture_index(55,rh_thresh = 50, method = 2), 0.00540259, tolerance = 0.00001)
})

# # see issue #22
# plot(latent_period(9:35), x = 9:35,
#      xlab = "Temperature C",
#      ylab = "days",
#      pch = 16)
