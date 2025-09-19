test_that("Check if the available attributes changed", {
  skip_if_offline(host = "opendata.shmu.sk")
  expect_identical(
    httr2::request("https://opendata.shmu.sk/meteorology/weather/radar/volume/skjav/") |>
      httr2::req_options(ssl_verifypeer = 0) |>
      httr2::req_perform() |>
      httr2::resp_body_html() |>
      xml2::xml_find_all("//a/@href") |>
      xml2::xml_text() |>
      grep(pattern = '/$', value = TRUE) |>
      grep(pattern = 'volume/$',invert = TRUE, value = TRUE) |>
      gsub(pattern = '/',replacement='')
    ,
      c("KDP", "PhiDP", "RhoHV", "V", "W", "ZDR", "dBZ", "dBuZ")
  )
})
test_that("Pvol for Romania can be downloaded", {
  skip_if_offline(host = "opendata.shmu.sk")
  time <- lubridate::floor_date(
    as.POSIXct(Sys.time(),
      tz = "Europe/Helsinki"
    ) - lubridate::hours(10), "5 mins"
  )
  pvol <- expect_s3_class(get_pvol("skjav",
    time,
    param = "all"
  ), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(
    lubridate::floor_date(pvol$datetime, "5 mins"),
    lubridate::with_tz(time, "UTC")
  )
})
