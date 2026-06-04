test_that("Pvol for hochficht in austria can be downloaded", {
  skip_if_offline("public.hub.geosphere.at")
  time <- as.POSIXct(Sys.time() - 10000, tz = "Europe/Helsinki")
  pvol <- expect_s3_class(get_pvol("hochficht", time, param = "all"), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(
    pvol$datetime,
    lubridate::floor_date(lubridate::with_tz(time, "UTC"), "5 min")
  )
})

test_that("Pvol for hochficht in austria can be downloaded", {
  skip_if_offline("public.hub.geosphere.at")
  time <- as.POSIXct(
    Sys.time() - 10000 - 24 * 3 * 60 * 60,
    tz = "Europe/Helsinki"
  )
  expect_error(
    get_pvol("hochficht", time, param = "all"),
    class = "getRad_error_get_pvol_at_no_data"
  )
})
test_that("hochficht can be combined with other radars", {
  expect_type(
    pvol_lst <- get_pvol(
      c("depro", "hochficht", "KABX"),
      as.POSIXct(Sys.Date())
    ),
    "list"
  )
  expect_all_true(purrr::map_lgl(pvol_lst, inherits, "pvol"))
})
