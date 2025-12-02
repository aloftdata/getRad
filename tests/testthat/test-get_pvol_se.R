skip_if_se_not_updated <- function(radar, time) {
  if (
    httr2::request(
      "https://opendata-download-radar.smhi.se/api/version/latest/area/"
    ) |>
      httr2::req_perform() |>
      httr2::resp_body_json() |>
      purrr::chuck("areas") |>
      purrr::map(~ as.data.frame(.x)) |>
      dplyr::bind_rows() |>
      dplyr::filter(key == radar) |>
      purrr::chuck("updated") |>
      lubridate::as_datetime() <
      time
  ) {
    testthat::skip(glue::glue(
      "Most recent data for {radar} is older then {time}"
    ))
  } else {
    invisible()
  }
}
test_that("Pvol for Sweden can be downloaded", {
  skip_if_offline("opendata-download-radar.smhi.se")
  time <- as.POSIXct(Sys.time(), tz = "Europe/Helsinki") - lubridate::hours(10)
  skip_if_se_not_updated("atvidaberg", time)
  expect_s3_class(pvol <- get_pvol("seatv", time, param = "all"), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(
    pvol$datetime,
    lubridate::with_tz(lubridate::floor_date(time, "5 mins"), "UTC")
  )
})

test_that("Pvol for Sweden fails out of time range", {
  skip_if_offline("opendata-download-radar.smhi.se")
  time <- Sys.time() - lubridate::hours(40)
  skip_if_se_not_updated("hudiksvall", time)

  expect_error(
    get_pvol("sehuv", time),
    class = "getRad_error_get_pvol_se_data_not_found"
  )
  time <- Sys.time() + lubridate::hours(1)
  expect_error(
    get_pvol("sehuv", time),
    class = "getRad_error_get_pvol_se_data_not_found"
  )
})

test_that("Pvol for Sweden fails incorrect radar", {
  skip_if_offline("opendata-download-radar.smhi.se")
  time <- Sys.time() - lubridate::hours(10)
  expect_error(get_pvol("sehut", time), class = "getRad_error_radar_not_found")
})
