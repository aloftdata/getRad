test_that("Pvol for uk can be downloaded", {
  skip_if_offline("ncas-radar-o.s3-ext.jc.rl.ac.uk")
  time <-
    as.POSIXct("2022-4-1 10:00:00", tz = "Europe/Helsinki")
  pvol <- expect_s3_class(get_pvol("ukjer", time, param = "all"), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(
    lubridate::floor_date(pvol$datetime, "5 mins"),
    lubridate::with_tz(time, "UTC")
  )
  pvolsp <- expect_s3_class(
    get_pvol("ukjer", pulse_type = "sp", time, param = "all"),
    "pvol"
  )
  expect_true(bioRad::is.pvol(pvolsp))
  expect_identical(
    lubridate::floor_date(pvolsp$datetime, "5 mins"),
    lubridate::with_tz(time, "UTC")
  )
  expect_false(identical(pvol, pvolsp))
})

test_that("plusetype argument", {
  expect_error(get_pvol("ukcas", Sys.time(), pulse_type = "ll"), "pulse_type")
  expect_error(get_pvol("ukcas", Sys.time(), pulse_type = 1), "pulse_type")
})
test_that("404 no data", {
  skip_if_offline("ncas-radar-o.s3-ext.jc.rl.ac.uk")
  expect_error(
    get_pvol("ukcas", as.POSIXct("2012-4-1 10:00:10")),
    class = "getRad_error_uk_no_data_404"
  )
  expect_error(
    get_pvol("ukcos", as.POSIXct("2012-4-1 10:00:10")),
    class = "getRad_error_radar_not_found"
  )
})

test_that("no sp data", {
  skip_if_offline("ncas-radar-o.s3-ext.jc.rl.ac.uk")
  time <-
    as.POSIXct("2022-4-1 10:05:10", tz = "Europe/Helsinki")
  expect_error(
    get_pvol("ukcas", time, pulse_type = "sp"),
    class = "getRad_error_uk_no_sp_data"
  )
})
