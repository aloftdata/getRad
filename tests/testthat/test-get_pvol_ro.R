test_that("Check if the available attributes changed", {
  skip_if_offline("opendata.meteoromania.ro")
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
    c("Height", "KDP", "RhoHV", "V", "ZDR", "dBR", "dBZ") |> sort()
    # Height and dBR are none polar files
  )
})
test_that("Pvol for Romania can be downloaded", {
  skip_if_offline("opendata.meteoromania.ro")
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

  withr::with_options(
    list(
      "getRad.ro_url" = "https://opendata.meteoromania.ro/radar/{toupper(substr(radar,3,5))}/{toupper(substr(radar,3,5))}_{strftime(time,'%Y%m%d%H%M', tz='UTC')}0400{params}.hdf",
      "getRad.ro_url_alt" = "https://opendata.meteoromania.ro/radar/{toupper(substr(radar,3,5))}/{toupper(substr(radar,3,5))}_{strftime(time,'%Y%m%d%H%M', tz='UTC')}0200{params}.hdf"
    ),
    expect_identical(get_pvol("romed", time, param = "all"), pvol)
  )
})
test_that("Pvol for Romania fails if urls are wrong", {
  withr::with_options(
    list(
      "getRad.ro_url" = "https://opendata.meteoromania.ro/radar/{toupper(substr(radar,3,5))}/{toupper(substr(radar,3,5))}_{strftime(time,'%Y%m%d%H%M', tz='UTC')}0400{params}.hdf",
      "getRad.ro_url_alt" = "https://opendata.meteoromania.ro/radar/{toupper(substr(radar,3,5))}/{toupper(substr(radar,3,5))}_{strftime(time,'%Y%m%d%H%M', tz='UTC')}0500{params}.hdf"
    ),
    expect_error(
      get_pvol("romed", time, param = "all"),
      class = "httr2_http_404"
    )
  )
})
