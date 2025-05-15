skip_if_offline(host = "opendata.meteo.be")

test_that("parse_numeric() properly returns NaN",{
  expect_identical(
    parse_numeric("nan"),
    NaN
  )
})

test_that("parse_numeric() returns a numeric",{
  expect_true(is.numeric(parse_numeric("42.336")))
})

test_that("parse_rmi() returns the expected values", {
  skip_if_offline(host = "opendata.meteo.be")

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
    ) |>
    # drop metadata header
    tail(-4)
  )

  parsed_rmi <- parse_rmi(fwf_text)

  expect_identical(
    parsed_rmi[1,"datetime"][[1]],
    lubridate::ymd("2020-01-24", tz = "UTC")
  )

  expect_identical(
    parsed_rmi[2,"height"][[1]],
    200L
  )

  expect_identical(
    parsed_rmi[2,"u"][[1]],
    0.58
  )

  expect_identical(
    parsed_rmi[2,"v"][[1]],
    1.29
  )

  expect_identical(
    parsed_rmi[2,"w"][[1]],
    6.25
  )

  expect_identical(
    parsed_rmi[2,"ff"][[1]],
    1.42
  )

  expect_identical(
    parsed_rmi[2,"dd"][[1]],
    24.3
  )

  expect_identical(
    parsed_rmi[2,"sd_vvp"][[1]],
    1.49
  )

  expect_identical(
    parsed_rmi[2,"gap"][[1]],
    FALSE
  )

  expect_identical(
    parsed_rmi[2,"dbz"][[1]],
    -10.38
  )

  expect_identical(
    parsed_rmi[2,"eta"][[1]],
    32.2
  )

  expect_identical(
    parsed_rmi[819,"dens"][[1]],
    0.54
  )

  expect_identical(
    parsed_rmi[3,"dbzh"][[1]],
    -6.170
  )

  expect_identical(
    parsed_rmi[2,"n"][[1]],
    1044L
  )

  expect_identical(
    parsed_rmi[1,"n_dbz"][[1]],
    829L
  )

  expect_identical(
    parsed_rmi[1,"n_all"][[1]],
    1593L
  )

  expect_identical(
    parsed_rmi[1,"n_dbz_all"][[1]],
    3233L

  expect_identical(
    parsed_rmi[1,"sd_vvp_threshold"][[1]],
    2
  )

  # Allow floating point errors.
  expect_equal(
    parsed_rmi[1,"rcs"][[1]],
    10.98804,
    tolerance = 0.00001
  )
})
