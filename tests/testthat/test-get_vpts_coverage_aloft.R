test_that("get_vpts_coverage_aloft() returns a tibble", {
  skip_if_offline()

  expect_s3_class(
    get_vpts_coverage_aloft(),
    "tbl_df"
  )
})

test_that("get_vpts_coverage_aloft() returns the expected columns", {
  skip_if_offline()

  expect_named(
    get_vpts_coverage_aloft(),
    c("directory", "file_count", "source", "radar", "date")
  )
})
