test_that("get_vpts_dark_ecology() returns error on invalid odim code", {})

test_that("get_vpts_dark_ecology() can read dark ecology data from disk", {
  dk_path <- system.file("extdata", "darkecology", package = "getRad")

  expect_type(
    kcbx_vpts <- get_vpts_dark_ecology(
      directory = dk_path,
      radar = "KCBX",
      rounded_interval = lubridate::interval(
        start = "20150101",
        end = "20150201"
      )
    ),
    "list"
  )

  purrr::map(
    kcbx_vpts,
    \(list_child) {
      expect_s3_class(list_child, "vp")
    }
  )
})

test_that("get_vpts_dark_ecology() supports reading multiple radars", {})

test_that("get_vpts_aloft() returns error when radar is not found", {})

test_that("get_vpts_dark_ecology() returns error when date is not found", {})
