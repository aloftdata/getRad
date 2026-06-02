birdcast_coverage <- tibble::tibble(
  radar = "KABR",
  date = as.Date(c("2013-09-01", "2013-09-02"))
)

test_that("get_vpts_birdcast() returns error on invalid radar code", {
  expect_error(
    getRad:::get_vpts_birdcast(
      radar = "KAB",
      rounded_interval = lubridate::interval("2013-09-01", "2013-09-02"),
      coverage = birdcast_coverage
    ),
    class = "getRad_error_radar_not_single_odim_nexrad"
  )

  expect_error(
    getRad:::get_vpts_birdcast(
      radar = 12345,
      rounded_interval = lubridate::interval("2013-09-01", "2013-09-02"),
      coverage = birdcast_coverage
    ),
    class = "getRad_error_radar_not_single_odim_nexrad"
  )
})

test_that("get_vpts_birdcast() returns error when multiple radars are queried", {
  expect_error(
    getRad:::get_vpts_birdcast(
      radar = c("KABR", "KABX"),
      rounded_interval = lubridate::interval("2013-09-01", "2013-09-02"),
      coverage = birdcast_coverage
    ),
    class = "getRad_error_radar_not_single_odim_nexrad"
  )
})

test_that("get_vpts_birdcast() returns error when radar is not found in coverage", {
  expect_error(
    getRad:::get_vpts_birdcast(
      radar = "ZZZZ",
      rounded_interval = lubridate::interval("2013-09-01", "2013-09-02"),
      coverage = birdcast_coverage
    ),
    class = "getRad_error_birdcast_radar_not_found"
  )

  expect_identical(
    rlang::catch_cnd(
      getRad:::get_vpts_birdcast(
        radar = "ZZZZ",
        rounded_interval = lubridate::interval("2013-09-01", "2013-09-02"),
        coverage = birdcast_coverage
      ),
      classes = "getRad_error_birdcast_radar_not_found"
    )$missing_radar,
    "ZZZZ"
  )
})

test_that("get_vpts_birdcast() returns error when date is requested not in coverage", {
  expect_error(
    getRad:::get_vpts_birdcast(
      radar = "KABR",
      rounded_interval = lubridate::interval("1900-01-01", "1900-01-02"),
      coverage = birdcast_coverage
    ),
    class = "getRad_error_date_not_found"
  )
})

test_that("get_vpts_birdcast() can fetch daily VPTS data from BirdCast archive", {
  skip_if_offline()

  birdcast_vpts_tbl <- getRad:::get_vpts_birdcast(
    radar = "KABR",
    rounded_interval = lubridate::interval("2013-09-01", "2013-09-02"),
    coverage = birdcast_coverage
  )

  expect_type(birdcast_vpts_tbl, "list")
  expect_s3_class(birdcast_vpts_tbl, "tbl_df")

  expect_named(
    birdcast_vpts_tbl,
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

  expect_true(nrow(birdcast_vpts_tbl) > 0)
  expect_true(all(birdcast_vpts_tbl$radar == "kabr"))
  expect_true(all(birdcast_vpts_tbl$source == "birdcast"))
})
test_that("get_vpts() can fetch daily VPTS data from BirdCast archive", {
  skip_if_offline()
  date <- as.Date("2026-4-1")
  vpts <- getRad:::get_vpts(
    radar = "KABX",
    date,
    source = "birdcast"
  )
  expect_s3_class(vpts, "vpts")
  expect_all_true(as.Date(vpts$datetime) == date)
  expect_false(vpts$regular)
})
