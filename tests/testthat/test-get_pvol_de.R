test_that("Pvol for German can be downloaded", {
  skip_if_offline(host = "opendata.dwd.de")
  time <- lubridate::floor_date(
    as.POSIXct(Sys.time(), tz = "Europe/Helsinki") - lubridate::hours(10),
    "5 mins"
  )
  pvol <- expect_s3_class(get_pvol("deess", time, param = "all"), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(
    lubridate::floor_date(pvol$datetime, "5 mins"),
    lubridate::with_tz(time, "UTC")
  )
})
test_that("Correct error for old German data", {
  skip_if_offline(host = "opendata.dwd.de")
  expect_error(
    get_pvol("deess", Sys.time() - lubridate::days(4)),
    class = "getRad_error_germany_unexpected_number_of_files"
  )
})
