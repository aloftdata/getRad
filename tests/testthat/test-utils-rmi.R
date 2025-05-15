skip_if_offline(host = "opendata.meteo.be")

test_that("get_datetime() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_datetime(fwf_text[6]),
    lubridate::ymd("2020-01-24", tz = "UTC")
  )
})

test_that("get_height() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_height(fwf_text[6]),
    200L
  )
})

test_that("get_u() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_u(fwf_text[6]),
    0.58
  )
})

test_that("get_u() can convert `'nan'` to NaN", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_u(fwf_text[5]),
    NaN
  )
})

test_that("get_v() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_v(fwf_text[6]),
    1.29
  )
})

test_that("get_w() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_w(fwf_text[6]),
    6.25
  )
})

test_that("get_ff() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_ff(fwf_text[6]),
    1.42
  )
})

test_that("get_dd() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_dd(fwf_text[6]),
    24.3
  )
})

test_that("get_sd_vvp() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_sd_vvp(fwf_text[6]),
    1.49
  )
})

test_that("get_gap() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_gap(fwf_text[6]),
    FALSE
  )
})

test_that("get_dbz() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_dbz(fwf_text[6]),
    -10.38
  )
})

test_that("get_eta() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_eta(fwf_text[6]),
    32.2
  )
})

test_that("get_dens() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_dens(fwf_text[6]),
    0
  )

  expect_identical(
    get_dens(fwf_text[3680]),
    11.10
  )
})

test_that("get_dbzh() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_dbzh(fwf_text[5]),
    26.95
  )
})

test_that("get_n() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_n(fwf_text[6]),
    1044L
  )
})

test_that("get_n_dbz() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_n_dbz(fwf_text[5]),
    829L
  )
})

test_that("get_n_all() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_n_all(fwf_text[5]),
    1593L
  )
})

test_that("get_n_dbz_all() returns the expected value", {
  fwf_text <- get0(
    "fwf_text",
    ifnotfound = vroom::vroom_lines(
      file.path(
        "https://opendata.meteo.be/ftp/observations",
        "radar/vbird",
        "bejab",
        "2020",
        "bejab_vpts_20200124.txt"
      )
    )
  )

  expect_identical(
    get_n_dbz_all(fwf_text[5]),
    3233L
  )
})
