test_that("get_vpts_coverage_nexrad() returns a tibble", {
  skip_if_offline()

  expect_s3_class(
    get_vpts_coverage_nexrad(),
    "tbl_df"
  )
})

test_that("get_vpts_coverage_nexrad() returns the expected columns", {
  skip_if_offline()

  expect_named(
    get_vpts_coverage_nexrad(),
    c("directory", "file_count", "source", "radar", "date")
  )
})

test_that("get_vpts_coverage_nexrad() returns expected NEXRAD values", {
  skip_if_offline()

  coverage <- get_vpts_coverage_nexrad()

  expect_true(all(coverage$source == "nexrad"))
  expect_s3_class(coverage$date, "Date")
  expect_true(all(grepl("^[A-Z0-9]{4}$", coverage$radar)))
  expect_true(all(grepl("^nexrad/daily/[A-Z0-9]{4}/[0-9]{4}/[0-9]{2}/[0-9]{2}$", coverage$directory)))
})