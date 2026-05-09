test_that("get_vpts_nexrad() can fetch daily VPTS data from BirdCast archive", {
  skip_if_offline()

  nexrad_vpts_tbl <- getRad:::get_vpts_nexrad(
    radar = "KABR",
    rounded_interval = lubridate::interval("2013-09-01", "2013-09-02")
  )

  expect_s3_class(
    nexrad_vpts_tbl,
    "tbl_df"
  )

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