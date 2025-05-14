time_utc <- lubridate::floor_date(Sys.time() - lubridate::hours(12), "5 mins")
dt_int   <- lubridate::interval(time_utc, time_utc + lubridate::minutes(10))

test_that("NEXRAD polar volume can be downloaded", {
  skip_if_offline(host = "noaa-nexrad-level2.s3.amazonaws.com")
  expect_s3_class(
    getRad::get_pvol("KABR", time_utc),
    "pvol"
  )
})

test_that("Mixed radar vector (single timestamp)", {
  skip_if_offline()
  pvols <- getRad::get_pvol(c("KABR", "czska"), time_utc)
  expect_true(is.list(pvols))
  expect_gt(length(pvols), 0)
  expect_true(all(purrr::map_lgl(pvols, ~ inherits(.x, "pvol"))))
})

test_that("Mixed radar vector + 10 minute interval", {
  skip_if_offline()
  pvols <- getRad::get_pvol(c("KABR", "czska"), dt_int)
  expect_true(is.list(pvols))
  expect_gt(length(pvols), 0)
  expect_true(all(purrr::map_lgl(pvols, ~ inherits(.x, "pvol"))))
})
