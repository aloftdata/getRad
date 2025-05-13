skip_if_offline()
fwf_text <- readr::read_lines("https://opendata.meteo.be/ftp/observations/radar/vbird/bejab/2020/bejab_vpts_20200124.txt")

test_that("get_datetime() returns the expected value", {
  expect_identical(
    get_datetime(fwf_text[6]),
    lubridate::ymd("2020-01-24", tz = "UTC")
  )
})
