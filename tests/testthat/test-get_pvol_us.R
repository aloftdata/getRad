time <- ymd_hms("2024-05-12 04:10:00", tz = "UTC")
test_that("nearest NEXRAD key is floored correctly", {
  key <- getRad:::.nearest_nexrad_key(time, "KABR")
  expect_equal(key, "2024/05/12/KABR/KABR20240512_040622_V06")
})

test_that("URL helper concatenates correctly", {
  key  <- "2024/05/12/KABR/KABR20240512_040622_V06"
  url  <- "https://noaa-nexrad-level2.s3.amazonaws.com/2024/05/12/KABR/KABR20240512_040622_V06"
  expect_equal(getRad::nexrad_key_to_url(key), url)
})

test_that("get_pvol_us downloads and reads a pvol", {
  pvol <- getRad::get_pvol_us("KABR", time)
  expect_true(inherits(pvol, "pvol"))
})
