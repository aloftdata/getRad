time_utc <- lubridate::floor_date(Sys.time() - lubridate::hours(12), "5 mins")
dt_int <- lubridate::interval(time_utc, time_utc + lubridate::minutes(9))

test_that("NEXRAD polar volume can be downloaded", {
  skip_if_offline(host = "noaa-nexrad-level2.s3.amazonaws.com")
  suppressMessages(
    expect_s3_class(
      getRad::get_pvol("KABR", time_utc),
      "pvol"
    )
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
  # with exact time match
  t <- as.POSIXct("2025-1-10 17:58:13", tz = "UTC")
  suppressMessages(expect_identical(
    getRad::get_pvol("KABX", t)$datetime,
    as.POSIXct("2025-01-10 17:58:13", tz = "UTC")
  ))
})

test_that("Mixed radar vector (single timestamp)", {
  skip_if_offline()
  suppressMessages(pvols <- getRad::get_pvol(c("KABR", "czska"), time_utc))
  expect_true(is.list(pvols))
  expect_gt(length(pvols), 0)
  expect_true(all(purrr::map_lgl(pvols, ~ inherits(.x, "pvol"))))
})
test_that("Correct error is given when no near data is found", {
  expect_error(get_pvol("KABX", as.POSIXct("1970-1-1")), class = "getRad_error_us_no_scan_found")
})

test_that("Mixed radar vector + 9 minute interval", {
  skip_if_offline()
  suppressMessages(pvols <- getRad::get_pvol(c("KABR", "czska"), dt_int))
  expect_true(is.list(pvols))
  expect_gt(length(pvols), 2)
  expect_true(all(purrr::map_lgl(pvols, ~ inherits(.x, "pvol"))))
})
test_that("Caching of keys works", {
  skip_on_cran()
  skip_if_offline()
  t <- as.POSIXct("2025-2-3 5:00")
  r <- "KGGW"
  expect_gt(system.time(.most_representative_nexrad_key(t, r))["elapsed"], .15)
  expect_true(all(c(
    "list_nexrad_keys_kggw_2025-02-04_historic", "list_nexrad_keys_kggw_2025-02-03_historic",
    "list_nexrad_keys_kggw_2025-02-02_historic"
  ) %in% getOption("getRad.cache")$keys()))
  expect_lt(system.time(.most_representative_nexrad_key(t, r))["elapsed"], .025)
})
