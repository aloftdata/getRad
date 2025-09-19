test_that("Pvol for Danish can be downloaded", {
  withr::local_options(list("keyring_backend" = "env"))
  skip_if(
    inherits(try(get_secret("dk_api_key"), silent = TRUE), "try-error"),
    message = "Because no key for Denmark is available in the testing environment"
  )
  time <- lubridate::floor_date(
    as.POSIXct(Sys.time(), tz = "Europe/Helsinki") - lubridate::hours(10),
    "5 mins"
  ) - lubridate::days(90)
  pvol <- expect_s3_class(get_pvol("dkbor", time, param = "all"), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(lubridate::floor_date(pvol$datetime, "5 mins"), lubridate::with_tz(time, "UTC"))
})
