coverage <- get_vpts_coverage_aloft()

test_that("get_vpts_aloft() returns error on invalid odim code", {
  # Radar is not 5 character string
  expect_error(
    get_vpts_aloft(
      radar = "beja",
      rounded_interval = lubridate::interval("2023-01-01", "2023-01-02"),
      source = "uva",
      coverage
    ),
    class = "getRad_error_radar_not_single_odim_string"
  )
  # radar is not a string
  expect_error(
    get_vpts_aloft(
      radar = 12345,
      rounded_interval = lubridate::interval("2023-01-01", "2023-01-02"),
      source = "uva",
      coverage
    ),
    class = "getRad_error_radar_not_single_odim_string"
  )
})

test_that("get_vpts_aloft() returns error when multiple radars are queried", {
  expect_error(
    get_vpts_aloft(
      radar = c("bejab", "depro"),
      rounded_interval = lubridate::interval("2023-01-01", "2023-01-02"),
      source = "uva",
      coverage
    ),
    class = "getRad_error_radar_not_single_odim_string"
  )
})

test_that("get_vpts_aloft() returns error when radar is not found in coverage", {
  expect_error(
    get_vpts_aloft(
      radar = "aaazz",
      rounded_interval = lubridate::interval("2023-01-01", "2023-01-02"),
      source = "baltrad",
      coverage
    ),
    class = "getRad_error_aloft_radar_not_found"
  )
  expect_identical(
    rlang::catch_cnd(get_vpts_aloft(
      radar = c("nlaaa"),
      rounded_interval = lubridate::interval("2023-01-01", "2023-01-02"),
      source = "baltrad",
      coverage
    ))$missing_radar,
    c("nlaaa")
  )
})

test_that("get_vpts_aloft() returns error when date is requested not in coverage", {
  expect_error(
    get_vpts_aloft(
      radar = "bejab",
      rounded_interval = lubridate::interval("2018-05-01", "2018-05-02"),
      source = "baltrad",
      coverage
    ),
    class = "getRad_error_date_not_found"
  )
})

test_that("get_vpts_aloft() can fetch vtps data from aloft", {
  skip_if_offline()

  aloft_vpts_tbl <- get_vpts_aloft(
    radar = c("depro"),
    rounded_interval = lubridate::interval("2024-08-12", "2024-08-13"),
    source = "baltrad"
  )
  # Test that a tibble is returned
  expect_type(
    aloft_vpts_tbl,
    "list"
  )

  expect_s3_class(
    aloft_vpts_tbl,
    "tbl_df"
  )

  # Test that the tibble has the expected columns
  expect_named(
    aloft_vpts_tbl,
    c(
      "source",
      "radar",
      "datetime",
      "height",
      "u",
      "v",
      "w",
      "ff",
      "dd",
      "sd_vvp",
      "gap",
      "eta",
      "dens",
      "dbz",
      "dbz_all",
      "n",
      "n_dbz",
      "n_all",
      "n_dbz_all",
      "rcs",
      "sd_vvp_threshold",
      "vcp",
      "radar_latitude",
      "radar_longitude",
      "radar_height",
      "radar_wavelength",
      "source_file"
    )
  )
})
