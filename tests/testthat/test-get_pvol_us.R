time_utc <- lubridate::floor_date(Sys.time() - lubridate::hours(12), "5 mins")
dt_int <- lubridate::interval(time_utc, time_utc + lubridate::minutes(9))

test_that("NEXRAD polar volume can be downloaded", {
  skip_if_offline(host = "noaa-nexrad-level2.s3.amazonaws.com")
  expect_s3_class(
    suppressMessages(getRad::get_pvol("KABR", time_utc)),
    "pvol"
  )
})

test_that("NEXRAD polar volume correct time is downloaded", {
  skip_if_offline(host = "noaa-nexrad-level2.s3.amazonaws.com")
  t <- as.POSIXct("2025-1-10 18:00:00", tz = "UTC")
  suppressMessages(expect_identical(
    getRad::get_pvol("KABX", t)$datetime,
    as.POSIXct("2025-01-10 17:58:13", tz = "UTC")
  ))
  # also test different tz
  t <- as.POSIXct("2023-1-10 12:00:00", tz = "US/Alaska")
  suppressMessages(expect_identical(
    getRad::get_pvol("KAMA", t)$datetime,
    as.POSIXct("2023-01-10 20:55:53", tz = "UTC")
  ))
})

test_that("Mixed radar vector (single timestamp)", {
  skip_if_offline()
  suppressMessages(pvols <- getRad::get_pvol(c("KABR", "czska"), time_utc))
  expect_true(is.list(pvols))
  expect_gt(length(pvols), 0)
  expect_true(all(purrr::map_lgl(pvols, ~ inherits(.x, "pvol"))))
})

test_that("Mixed radar vector + 9 minute interval", {
  skip_if_offline()
  suppressMessages(pvols <- getRad::get_pvol(c("KABR", "czska"), dt_int))
  expect_true(is.list(pvols))
  expect_gt(length(pvols), 2)
  expect_true(all(purrr::map_lgl(pvols, ~ inherits(.x, "pvol"))))
})
