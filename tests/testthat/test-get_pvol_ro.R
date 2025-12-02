test_that("Check if the available attributes changed", {
  skip_if_offline()
  expect_identical(
    httr2::request("https://opendata.meteoromania.ro/radar/MED/") |>
      httr2::req_perform() |>
      httr2::resp_body_html() |>
      xml2::xml_find_all("//a/@href") |>
      xml2::xml_text() |>
      tail(-1) |>
      gsub(pattern = "MED_[0-9]*00", replacement = "") |>
      unique() |>
      gsub(pattern = ".hdf", replacement = "") |>
      sort(),
    c("KDP", "RhoHV", "V", "ZDR", "dBZ") |> sort()
  )
})
test_that("Pvol for Romania can be downloaded", {
  skip_if_offline()
  time <- lubridate::floor_date(
    as.POSIXct(Sys.time(), tz = "Europe/Helsinki") - lubridate::hours(10),
    "5 mins"
  )
  pvol <- expect_s3_class(get_pvol("romed", time, param = "all"), "pvol")
  expect_true(bioRad::is.pvol(pvol))
  expect_identical(
    lubridate::floor_date(pvol$datetime, "5 mins"),
    lubridate::with_tz(time, "UTC")
  )
})
