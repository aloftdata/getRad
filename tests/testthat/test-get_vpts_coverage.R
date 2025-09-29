test_that("Source argument as expected", {
  expect_error(
    get_vpts_coverage(source = NULL),
    "must be a character vector, not `NULL`."
  )
  expect_error(get_vpts_coverage(source = "asdf"), ' not "asdf"')
  expect_error(
    get_vpts_coverage(source = character()),
    class = "getRad_error_length_zero"
  )
})

test_that("format as expect for aloft", {
  skip_if_offline()

  data <- get_vpts_coverage("uva")
  expect_true(all(c("source", "radar", "date") %in% names(data)))
  expect_s3_class(data$date, "Date")
  expect_true(all(is_odim(data$radar)))
})

test_that("format as expect for rmi", {
  skip_if_offline("opendata.meteo.be")

  data <- get_vpts_coverage("rmi")
  expect_true(all(c("source", "radar", "date") %in% names(data)))
  expect_s3_class(data$date, "Date")
  expect_true(all(is_odim(data$radar)))
})


test_that("combined retrieval works", {
  skip_if_offline("opendata.meteo.be")

  data <- get_vpts_coverage(c("rmi", "ecog-04003"))
  expect_true(all(c("source", "radar", "date") %in% names(data)))
  expect_s3_class(data$date, "Date")
  expect_identical(unique(data$source), c("rmi", "ecog-04003"))
  expect_true(all(is_odim(data$radar)))
})

test_that("get_vpts_coverage() returns 'baltrad' as a default source", {
  expect_identical(
    unique(get_vpts_coverage()$source),
    "baltrad"
  )
})


test_that("The argument source='all' returns all data", {
  expect_equal(
    get_vpts_coverage(source = "all") |>
      dplyr::pull(source) |>
      table(),
    get_vpts_coverage(
      source = eval(rlang::fn_fmls(get_vpts_coverage)$source)
    ) |>
      dplyr::pull(source) |>
      table()
  )
})
