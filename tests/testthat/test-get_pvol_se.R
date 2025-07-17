test_that("Pvol for Sweden can be downloaded", {
  time <- as.POSIXct(Sys.time(), tz = "Europe/Helsinki") - lubridate::hours(10)
  expect_s3_class(pvol <- get_pvol("seatv", time, param = "all"), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(
    pvol$datetime,
    lubridate::with_tz(lubridate::floor_date(time, "5 mins"), "UTC")
  )
})

test_that("Pvol for Sweden fails out of time range", {
  time <- Sys.time() - lubridate::hours(40)
  expect_error(get_pvol("sehuv", time), class = "getRad_error_get_pvol_se_data_not_found")
  time <- Sys.time() + lubridate::hours(1)
  expect_error(get_pvol("sehuv", time), class = "getRad_error_get_pvol_se_data_not_found")
})

test_that("Pvol for Sweden fails incorrect radar", {
  time <- Sys.time() - lubridate::hours(10)
  expect_error(get_pvol("sehut", time), class = "getRad_error_get_pvol_se_radar_not_found")
})
