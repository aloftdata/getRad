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

})

test_that("get_vpts_rmi() supports intervals passing a year boundary", {

})

test_that("get_vpts_rmi() returns rmi as the source", {

})

test_that("get_vpts_rmi() includes a radar column", {

})

test_that("get_vpts_rmi() includes a source_file column", {

})

test_that("get_vpts_rmi() returns error if radar date combo is not found", {
  expect_error(
    get_vpts_rmi("bejab",
      rounded_interval = lubridate::interval("3030-01-01", "3031-01-01")
    ),
    class = "getRad_error_date_not_found"
  )
})


