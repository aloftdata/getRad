test_that("Pvol for estonia can be downloaded", {
  skip_if_offline()
  withr::local_options(list(httr2_progress = FALSE))

  time <- as.POSIXct("2024-4-4 21:00:00", tz = "Europe/Helsinki")

  # The API frequently sends 429/500 responses, therefore this test is allowed
  # to skip when the download is unsuccessful.
  pvol <- tryCatch(
    get_pvol("eesur", time, param = "all"),
    error = function(e) NULL
  )

  skip_if_not(
    inherits(pvol, "pvol"),
    message = paste(
      "PVOL download for Estonia was unsuccessful;",
      "success is variable in testing environments"
    )
  )

  expect_s3_class(pvol, "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(pvol$datetime, lubridate::with_tz(time, "UTC"))
})
