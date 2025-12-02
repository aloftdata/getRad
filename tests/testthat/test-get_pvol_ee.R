test_that("Pvol for estonia can be downloaded", {
  skip_if_offline()
  withr::local_options(list(httr2_progress = FALSE))
  # The api frequently sends a 429 response therefore test is allowed to fail
  show_failure(expect_no_error(
    pvol <- get_pvol(
      "eesur",
      time <- as.POSIXct("2024-4-4 21:00:00", tz = "Europe/Helsinki"),
      param = "all"
    )
  ))
  ## If get_pvol() returns an error, the other tests are skipped.
  skip_if_not(
    inherits(pvol, "pvol"),
    message = "PVOL download for estonia was unsuccesful, succes is variable in testing environments"
  )
  expect_s3_class(pvol, "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(pvol$datetime, lubridate::with_tz(time, "UTC"))
})
