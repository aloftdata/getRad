test_that("rmi_data_coverage() returns a tibble", {
  skip_if_offline("data.meteo.be")

  expect_s3_class(rmi_data_coverage(), "tbl_df")
})

test_that("rmi_data_coverage() returns expected columns", {
  expect_named(
    rmi_data_coverage(),
    c(
      "directory",
      "file",
      "radar",
      "date"
    )
  )
})

test_that("rmi_data_coverage() returns known radars and years", {
  cov <- rmi_data_coverage()
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

test_that("rmi_data_coverage() allows selection on year", {
  expect_identical(
    unique(lubridate::year(rmi_data_coverage(year = 2022)$date)),
    2022
  )
})

test_that("rmi_data_coverage() allows selection on radar", {
  expect_identical(
    unique(rmi_data_coverage(radar = "bejab")$radar),
    "bejab"
  )
})
