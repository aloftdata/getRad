dk_path <- system.file("extdata", "darkecology", package = "getRad")

skip_if(dk_path == "")
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
    class = "getRad_error_vpts_dark_ecology_no_files"
  )
  expect_warning(
    get_vpts(
      path = dk_path,
      radar = c("KCBA", "KCBX"),
      lubridate::interval(
        start = "20150101",
        end = "20150201"
      ),
      source = "dark_ecology"
    ),
    class = "getRad_warning_vpts_dark_ecology_no_files_for_some_radars"
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


test_that("get_vpts_dark_ecology() works if `{fs}` is not installed", {
  expect_identical(
    vpts <- with_mocked_bindings(
      code = {
        get_vpts(
          path = dk_path,
          source = "dark_ecology",
          radar = "KCBX",
          lubridate::interval(
            start = "20150101",
            end = "20150201"
          )
        )
      },
      is_installed = \(x) FALSE
    ),
    get_vpts(
      path = dk_path,
      source = "dark_ecology",
      radar = "KCBX",
      lubridate::interval(
        start = "20150101",
        end = "20150201"
      )
    )
  )
})


test_that("get_vpts_dark_ecology() returns data in interval", {
  int <- lubridate::interval(
    start = "20150101 04:35:00",
    end = "20150201 09:00:22"
  )
  int_small <- lubridate::interval(
    start = "20150107 04:35:00",
    end = "20150107 09:00:22"
  )
  expect_identical(
    kcbx_vpts <- get_vpts(
      path = dk_path,
      radar = "KCBX",
      int,
      source = "dark_ecology"
    ),
    get_vpts(
      path = dk_path,
      radar = "KCBX",
      as.Date("2015-1-7"),
      source = "dark_ecology"
    )
  )
  expect_s3_class(
    kcbx_vpts_small <- get_vpts(
      path = dk_path,
      radar = "KCBX",
      int_small,
      source = "dark_ecology"
    ),
    'vpts'
  )
  expect_all_true(lubridate::`%within%`(kcbx_vpts_small$datetime, int_small))
  expect_length(
    kcbx_vpts_small$datetime,
    sum(lubridate::`%within%`(kcbx_vpts$datetime, int_small))
  )
})
