test_that("get_vpts_coverage_rmi() returns a tibble", {
  skip_if_offline("opendata.meteo.be")

  expect_s3_class(get_vpts_coverage_rmi(), "tbl_df")
})

test_that("get_vpts_coverage_rmi() returns expected columns", {
  skip_if_offline("opendata.meteo.be")

  expect_named(
    get_vpts_coverage_rmi(),
    c(
      "directory",
      "file",
      "radar",
      "date",
      "source"
    )
  )
})

test_that("get_vpts_coverage_rmi() returns known radars and years", {
  skip_if_offline("opendata.meteo.be")

  cov <- get_vpts_coverage_rmi()
  expect_in(
    cov$radar,
    c(
      "behel",
      "bejab",
      "bewid",
      "bezav",
      "deess",
      "denhb",
      "frabb",
      "frave",
      "nldhl",
      "nlhrw"
    )
  )

  expect_in(
    lubridate::year(cov$date),
    seq(2019, 2025)
  )
})

test_that("get_vpts_coverage_rmi() allows selection on year", {
  skip_if_offline("opendata.meteo.be")

  expect_identical(
    unique(lubridate::year(get_vpts_coverage_rmi(year = 2022)$date)),
    2022
  )
})

test_that("get_vpts_coverage_rmi() allows selection on radar", {
  skip_if_offline("opendata.meteo.be")

  expect_identical(
    unique(get_vpts_coverage_rmi(radar = "bejab")$radar),
    "bejab"
  )
})
