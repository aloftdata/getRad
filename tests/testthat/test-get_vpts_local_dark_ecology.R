dk_path <- system.file("extdata", "darkecology", package = "getRad")


test_that("get_vpts_dark_ecology() returns error on invalid odim code", {
  expect_error(
    get_vpts(
      path = dk_path,
      radar = "KCBA",
      lubridate::interval(
        start = "20150101",
        end = "20150201"
      ),
      source = "dark_ecology"
    ),
    class = "getRad_error_vpts_not_supported_return_type"
  )
})

test_that("get_vpts_dark_ecology() can read dark ecology data from disk and returns expected type", {
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

  expect_error(
    get_vpts(
      path = dk_path,
      return_type = "tibble",
      radar = "KCBX",
      lubridate::interval(
        start = "20150101",
        end = "20150201"
      ),
      source = "dark_ecology"
    ),
    class = "getRad_error_vpts_not_supported_return_type"
  )
})

test_that("get_vpts_dark_ecology() supports reading multiple radars", {
  radars <- c("KCBX", "KFDR")
  expect_type(
    vpts_lst <- get_vpts(
      path = dk_path,
      radar = radars,
      lubridate::interval(
        start = "20150101",
        end = "20150401"
      ),
      source = "dark_ecology"
    ),
    "list"
  )
  expect_named(vpts_lst, radars)
  expect_all_true(purrr::map_lgl(vpts_lst, inherits, "vpts"))
  expect_identical(
    purrr::pluck(vpts_lst, radars[1]),
    get_vpts(
      path = dk_path,
      radar = radars[1],
      lubridate::interval(
        start = "20150101",
        end = "20150401"
      ),
      source = "dark_ecology"
    )
  )
  expect_identical(
    purrr::pluck(vpts_lst, radars[2]),
    get_vpts(
      path = dk_path,
      radar = radars[2],
      lubridate::interval(
        start = "20150101",
        end = "20150401"
      ),
      source = "dark_ecology"
    )
  )
  expect_length(
    purrr::pluck(vpts_lst, radars[2], "datetime"),
    sum(grepl(pattern = radars[2], list.files(dk_path, recursive = T)))
  )
  expect_length(
    purrr::pluck(vpts_lst, radars[1], "datetime"),
    sum(grepl(pattern = radars[1], list.files(dk_path, recursive = T)))
  )
})

test_that("get_vpts_aloft() returns error when radar is not found", {})

test_that("get_vpts_dark_ecology() returns error when date is not found", {})

test_that("get_vpts_dark_ecology() works if `{fs}` is not installed", {})
