test_that("get_vpts_dark_ecology() returns error on invalid odim code", {})

test_that("get_vpts_dark_ecology() can read dark ecology data from disk", {
  dk_path <- system.file("extdata", "darkecology", package = "getRad")

  expect_s3_class(
    kcbx_vpts <- get_vpts(
      path = dk_path,
      radar = "KCBX",
      lubridate::interval(
        start = "20150101",
        end = "20150201"
      ),
      source = "dark_ecology"
    ),
    "vpts"
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

test_that("get_vpts_dark_ecology() works if `{fs}` is not installed", {})


kcbx_vpts <- get_vpts(
  path = dk_path,
  radar = c("KCBX", "KFDR"),
  lubridate::interval(
    start = "20150101",
    end = "20150401"
  )
)
