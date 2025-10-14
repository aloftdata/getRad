check_attributes <- function(x, wr_df) {
  expect_s3_class(x, "pvol")
  expect_true(x$radar %in% wr_df$radar)
  radar_df <- wr_df[
    x$radar == wr_df$radar &
      (as.logical(wr_df$status) | wr_df$country == "United States"),
  ]
  expect_equal(nrow(radar_df), 1)
  expect_equal(x$geo$height, radar_df$heightantenna, tolerance = .001)
  expect_equal(
    x$attributes$where$height,
    radar_df$heightantenna,
    tolerance = .001
  )
  expect_equal(x$geo$lat, radar_df$latitude, tolerance = .001)
  expect_equal(x$attributes$where$lat, radar_df$latitude, tolerance = .001)
  expect_equal(x$geo$lon, radar_df$longitude, tolerance = .001)
  expect_equal(x$attributes$where$lon, radar_df$longitude, tolerance = .001)
  expect_equal(
    x$attributes$how$wavelength,
    299792458 / (radar_df$frequency * 10^9) * 100,
    tolerance = .005
  )
}
wr_df <- get_weather_radars(source = c("nexrad", "opera"))
t <- Sys.time() - 24 * 60 * 60

test_that("Check Germany", {
  skip_on_cran()
  skip_on_ci()
  pv <- get_pvol("depro", t)
  check_attributes(pv, wr_df)
})

test_that("Check Czechia", {
  skip_on_cran()
  skip_on_ci()

  pv <- get_pvol("czbrd", t)
  check_attributes(pv, wr_df)
})

test_that("Check Estonia", {
  skip_on_cran()
  skip_on_ci()
  pv <- get_pvol("eehar", t)
  check_attributes(pv, wr_df)
})

test_that("Check Finland", {
  skip_on_cran()
  skip_on_ci()
  pv <- get_pvol("fikor", t)
  check_attributes(pv, wr_df)
})

test_that("Check Romania", {
  skip_on_cran()
  skip_on_ci()
  pv <- get_pvol("roora", t)
  check_attributes(pv, wr_df)
})

test_that("Check Slovakia", {
  skip_on_cran()
  skip_on_ci()
  pv <- get_pvol("skkoj", t)
  check_attributes(pv, wr_df)
})

test_that("Check Netherland", {
  skip_on_cran()
  skip_on_ci()
  withr::local_options(list(
    "keyring_backend" = "env"
  ))
  # get public key here https://developer.dataplatform.knmi.nl/open-data-api#token
  withr::local_envvar(
    list(
      "getRad_nl_api_key" = "eyJvcmciOiI1ZTU1NGUxOTI3NGE5NjAwMDEyYTNlYjEiLCJpZCI6ImVlNDFjMWI0MjlkODQ2MThiNWI4ZDViZDAyMTM2YTM3IiwiaCI6Im11cm11cjEyOCJ9"
    )
  )
  skip_if(Sys.which("KNMI_vol_h5_to_ODIM_h5") == "")
  pv <- get_pvol("nldhl", t)
  check_attributes(pv, wr_df)
})

test_that("Check Denmark", {
  skip_on_cran()
  skip_on_ci()
  skip_if(
    inherits(try(get_secret("dk_api_key"), silent = TRUE), "try-error"),
    message = "Because no key for Denmark is available in the testing environment"
  )
  pv <- get_pvol("dkbor", t)
  check_attributes(pv, wr_df)
})


test_that("Check US", {
  skip_on_cran()
  skip_on_ci()
  pv <- get_pvol("KABR", t)
  check_attributes(pv, wr_df)
})
