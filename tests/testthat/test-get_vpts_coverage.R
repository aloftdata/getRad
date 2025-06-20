test_that("Source argument as expected", {
  expect_error(get_vpts_coverage(source = NULL), "must be a character vector, not `NULL`.")
  expect_error(get_vpts_coverage(source = "asdf"), ' not "asdf"')
  expect_error(get_vpts_coverage(source = character()), class = "getRad_error_length_zero")
})
test_that("format as expect for aloft", {
  data <- get_vpts_coverage("uva")
  expect_true(all(c("source", "radar", "date") %in% names(data)))
  expect_s3_class(data$date, "Date")
  expect_true(all(is_odim(data$radar)))
})

test_that("format as expect for rmi", {
  data <- get_vpts_coverage("rmi")
  expect_true(all(c("source", "radar", "date") %in% names(data)))
  expect_s3_class(data$date, "Date")
  expect_true(all(is_odim(data$radar)))
})
