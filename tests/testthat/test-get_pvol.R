# Country specific tests are in the respective country specific `get_pvol_`
# files

test_that("get_pvol radar argument", {
  expect_error(get_pvol(), class = "getRad_error_radar_not_odim_nexrad")
  expect_error(
    get_pvol(1L, datetime = as.POSIXct(Sys.Date())),
    class = "getRad_error_radar_not_odim_nexrad"
  )
  expect_error(
    get_pvol("nldhlu", datetime = as.POSIXct(Sys.Date())),
    class = "getRad_error_radar_not_odim_nexrad"
  )
  expect_error(
    get_pvol(c("nlhrw", "nldhlu"), datetime = as.POSIXct(Sys.Date())),
    class = "getRad_error_radar_not_odim_nexrad"
  )
  expect_error(
    get_pvol(c("nlhrw", "nldhl", "nlhrw"), datetime = as.POSIXct(Sys.Date())),
    class = "getRad_error_radar_duplicated"
  )
  expect_error(
    get_pvol("nnhrw", datetime = as.POSIXct(Sys.Date())),
    class = "getRad_error_no_function_for_radar_with_country_code"
  )
})

test_that("get_pvol time argument", {
  expect_error(
    get_pvol("nlhrw", datetime = "asdf"),
    class = "getRad_error_time_not_correct"
  )
  expect_error(
    get_pvol("nlhrw"),
    class = "getRad_error_time_not_correct"
  )
  expect_error(
    get_pvol("nlhrw", datetime = 1L),
    class = "getRad_error_time_not_correct"
  )
  expect_error(
    get_pvol("nlhrw", datetime = Sys.Date()),
    class = "getRad_error_time_not_correct"
  )
  expect_error(
    get_pvol("nlhrw", datetime = as.POSIXct(Sys.Date())[c(1, 1)]),
    class = "getRad_error_duplicated_timestamps"
  )
})

test_that("multiple radars work", {
  skip_if_offline()
  multiple_radars <- c("dkbor", "dehnr")
  expect_type(
    pvl <- get_pvol(
      radar = multiple_radars,
      datetime = as.POSIXct(Sys.Date())
    ),
    "list"
  )

  expect_true(all(unlist(lapply(pvl, bioRad::is.pvol))))
  expect_identical(
    purrr::map_chr(pvl, ~ purrr::chuck(.x, "radar")),
    multiple_radars
  )
})

test_that("multiple timestamps work", {
  skip_if_offline()
  multiple_timestamps <-
    lubridate::ymd_hms(
      paste(
        lubridate::today(tzone = "UTC"),
        "00:45:00"
      ),
      paste(
        lubridate::today(tzone = "UTC"),
        "00:55:00"
      )
    )
  expect_type(
    pvl <- get_pvol(
      c("dkbor"),
      datetime = multiple_timestamps
    ),
    "list"
  )
  expect_true(all(unlist(lapply(pvl, bioRad::is.pvol))))
  expect_identical(
    lapply(pvl, \(x) x$datetime),
    as.list(seq(min(multiple_timestamps), max(multiple_timestamps), "5 mins"))
  )
})

test_that("multiple timestamps and radars work", {
  skip_if_offline()
  multiple_radars <- c("dkbor", "deess")
  multiple_timestamps <-
    lubridate::ymd_hms(
      paste(
        lubridate::today(tzone = "UTC"),
        "00:35:00"
      ),
      paste(
        lubridate::today(tzone = "UTC"),
        "00:45:00"
      )
    )
  expect_type(
    pvl <- get_pvol(
      radar = multiple_radars,
      datetime = multiple_timestamps
    ),
    "list"
  )
  expect_true(all(unlist(lapply(pvl, bioRad::is.pvol))))
  expect_identical(
    lapply(pvl, \(x) x$datetime),
    as.list(rep(
      seq(min(multiple_timestamps), max(multiple_timestamps), "5 mins"),
      2
    ))
  )
  expect_identical(
    lapply(pvl, \(x) x$radar),
    as.list(rep(multiple_radars, each = 3))
  )
})

test_that("Mixed radar vector (single timestamp)", {
  skip_if_offline()
  time_utc <- lubridate::as_datetime("2021-01-20 05:01:00")
  suppressMessages(pvols <- getRad::get_pvol(c("KABR", "finur"), time_utc))
  expect_true(is.list(pvols))
  expect_length(pvols, 2)
  expect_true(all(purrr::map_lgl(pvols, ~ inherits(.x, "pvol"))))
  expect_equal(
    purrr::map_chr(pvols, ~ .x$radar),
    c("KABR", "finur"),
    ignore_attr = TRUE
  )
  expect_identical(
    pvols[[1]]$datetime,
    lubridate::as_datetime("2021-01-20 04:57:36 UTC")
  )
  expect_identical(
    pvols[[2]]$datetime,
    lubridate::floor_date(time_utc, "5 mins")
  )
})

test_that("Mixed radar vector + 9 minute interval", {
  skip_if_offline()
  time_utc <- lubridate::as_datetime("2025-01-20 03:55:50")
  dt_int <- lubridate::interval(time_utc, time_utc + lubridate::minutes(9))
  suppressMessages(pvols <- getRad::get_pvol(c("KABR", "fikan"), dt_int))
  expect_type(pvols, "list")
  expect_length(pvols, 3)
  purrr::walk(pvols, ~ expect_s3_class(.x, "pvol"))
  expect_equal(
    purrr::map_chr(pvols, ~ .x$radar),
    c("KABR", "KABR", "fikan"),
    ignore_attr = T
  )
  expect_true(all(purrr::map_vec(pvols, ~ .x$datetime) %within% dt_int))
})
