test_that("get_vpts_rmi() can return vpts data for a single radar", {
  skip_if_offline()

  rmi_vpts_tbl <-
    get_vpts_rmi("bejab",
                 lubridate::interval("20200119", "20200124"))

  # Test that a tibble is returned
  expect_type(
    rmi_vpts_tbl,
    "list"
  )

  expect_s3_class(
    rmi_vpts_tbl,
    "tbl_df"
  )

})

test_that("get_vpts_rmi() returns the expected columns", {
  skip_if_offline()

  expected_columns <- c(
    "source",
    "datetime",
    "height",
    "u",
    "v",
    "w",
    "ff",
    "dd",
    "sd_vpp",
    "gap",
    "dbz",
    "eta",
    "dens",
    "dbzh",
    "n",
    "n_dbz",
    "n_all",
    "n_dbz_all",
    "source_file",
    "radar",
    "radar_latitude",
    "radar_longitude",
    "radar_height",
    "radar_wavelength"
  )

  rmi_vpts_tbl <-
    get_vpts_rmi("frave",
                 lubridate::interval("20240807", "20240810"))

  expect_named(
    rmi_vpts_tbl,
    expected_columns
  )

})

test_that("get_vpts_rmi() supports intervals passing a year boundary", {
  skip_if_offline()

  rmi_vpts_tbl_multi_year <-
    get_vpts_rmi("frave",
                 lubridate::interval("20231222", "20240110"))

  # Check that a tibble was returned (and no error)
  expect_s3_class(
    rmi_vpts_tbl_multi_year,
    "tbl_df"
  )

  # Check that the requested years are present
  expect_identical(
    unique(lubridate::year(rmi_vpts_tbl_multi_year$datetime)),
    c(2023L, 2024L)
  )
})

test_that("get_vpts_rmi() returns rmi as the source", {
  skip_if_offline()

  rmi_vpts_tbl <-
    get_vpts_rmi("bejab",
                 lubridate::interval("20200119", "20200124"))

  expect_identical(
    unique(rmi_vpts_tbl$source),
    "rmi"
  )
})

test_that("get_vpts_rmi() returns error if radar date combo is not found", {
  skip_if_offline()

  expect_error(
    get_vpts_rmi("bejab",
      rounded_interval = lubridate::interval("3030-01-01", "3031-01-01")
    ),
    class = "getRad_error_date_not_found"
  )
})


