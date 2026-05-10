nexrad_coverage <- tibble::tibble(
  radar = "KABR",
  date = as.Date(c("2013-09-01", "2013-09-02"))
)

test_that("get_vpts_nexrad() returns error on invalid radar code", {
  expect_error(
    getRad:::get_vpts_nexrad(
      radar = "KAB",
      rounded_interval = lubridate::interval("2013-09-01", "2013-09-02"),
      coverage = nexrad_coverage
    ),
    class = "getRad_error_radar_not_single_odim_nexrad"
  )

  expect_error(
    getRad:::get_vpts_nexrad(
      radar = 12345,
      rounded_interval = lubridate::interval("2013-09-01", "2013-09-02"),
      coverage = nexrad_coverage
    ),
    class = "getRad_error_radar_not_single_odim_nexrad"
  )
})

test_that("get_vpts_nexrad() returns error when multiple radars are queried", {
  expect_error(
    getRad:::get_vpts_nexrad(
      radar = c("KABR", "KABX"),
      rounded_interval = lubridate::interval("2013-09-01", "2013-09-02"),
      coverage = nexrad_coverage
    ),
    class = "getRad_error_radar_not_single_odim_nexrad"
  )
})

test_that("get_vpts_nexrad() returns error when radar is not found in coverage", {
  expect_error(
    getRad:::get_vpts_nexrad(
      radar = "ZZZZ",
      rounded_interval = lubridate::interval("2013-09-01", "2013-09-02"),
      coverage = nexrad_coverage
    ),
    class = "getRad_error_nexrad_radar_not_found"
  )

  expect_identical(
    rlang::catch_cnd(
      getRad:::get_vpts_nexrad(
        radar = "ZZZZ",
        rounded_interval = lubridate::interval("2013-09-01", "2013-09-02"),
        coverage = nexrad_coverage
      ),
      classes = "getRad_error_nexrad_radar_not_found"
    )$missing_radar,
    "ZZZZ"
  )
})

test_that("get_vpts_nexrad() returns error when date is requested not in coverage", {
  expect_error(
    getRad:::get_vpts_nexrad(
      radar = "KABR",
      rounded_interval = lubridate::interval("1900-01-01", "1900-01-02"),
      coverage = nexrad_coverage
    ),
    class = "getRad_error_date_not_found"
  )
})

test_that("get_vpts_nexrad() can fetch daily VPTS data from BirdCast archive", {
  skip_if_offline()

  nexrad_vpts_tbl <- getRad:::get_vpts_nexrad(
    radar = "KABR",
    rounded_interval = lubridate::interval("2013-09-01", "2013-09-02"),
    coverage = nexrad_coverage
  )

  expect_type(nexrad_vpts_tbl, "list")
  expect_s3_class(nexrad_vpts_tbl, "tbl_df")

  expect_named(
    nexrad_vpts_tbl,
    c(
      "radar",
      "datetime",
      "height",
      "height_reference",
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
      "source_file",
      "source"
    )
  )

  expect_true(nrow(nexrad_vpts_tbl) > 0)
  expect_true(all(nexrad_vpts_tbl$radar == "kabr"))
  expect_true(all(nexrad_vpts_tbl$source == "nexrad"))
})
