test_that("get_vpts_coverage_birdcast() returns a tibble", {
  skip_if_offline()
  expect_s3_class(
    get_vpts_coverage_birdcast(),
    "tbl_df"
  )
})

test_that("get_vpts_coverage_birdcast() returns the expected columns", {
  skip_if_offline()

  expect_named(
    get_vpts_coverage_birdcast(),
    c("directory", "file_count", "source", "radar", "date")
  )
})

test_that("get_vpts_coverage_birdcast() returns expected NEXRAD values", {
  skip_if_offline()

  coverage <- get_vpts_coverage_birdcast()

  expect_all_true(coverage$source == "birdcast")
  expect_s3_class(coverage$date, "Date")
  expect_true(all(grepl("^[A-Z0-9]{4}$", coverage$radar)))
  expect_true(all(grepl(
    "^nexrad/daily/[A-Z0-9]{4}/[0-9]{4}/[0-9]{2}/[0-9]{2}$",
    coverage$directory
  )))
})
