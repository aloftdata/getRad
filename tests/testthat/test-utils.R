test_that("odim test", {
  expect_true(is_odim("nlhrw"))
  expect_false(is_odim(Sys.Date()))
  expect_false(is_odim(Sys.time()))
  expect_false(is_odim("nlhr1"))
  expect_false(is_odim("nlhrww"))
  expect_false(is_odim("nlhr"))
  expect_false(is_odim(NA_character_))
  expect_false(is_odim(character(0L)))
  expect_identical(is_odim(c("nlhrw", "nldhl")), c(TRUE, TRUE))

  expect_false(is_odim_scalar(c("nlhrw", "nldhl")))

  expect_identical(is_odim(c("nlhrw", "nldhl2")), c(TRUE, FALSE))
  expect_error(
    check_odim(c("nlhrw", "nldhlu")),
    class = "getRad_error_radar_not_odim_string"
  )
})
test_that("fetch_from_url_raw warns on failing url", {
  expect_warning(
    res <- fetch_from_url_raw(
      c(
        "https://aloftdata.s3-eu-west-1.amazonaws.com/baltrad/daily/bejab/2024/bejab_vpts_20240347.csv"
      )
    ),
    class = "getRad_warning_404_on_csv_download"
  )
  expect_identical(
    res,
    list(raw())
  )

  expect_warning(
    res <- read_vpts_from_url(
      c(
        "https://aloftdata.s3-eu-west-1.amazonaws.com/baltrad/daily/bejab/2024/bejab_vpts_20240307.csv",
        "https://aloftdata.s3-eu-west-1.amazonaws.com/baltrad/daily/bejab/2024/bejab_vpts_20240347.csv"
      )
    ),
    class = "getRad_warning_404_on_csv_download"
  )
  expect_identical(
    res[2],
    list(data.frame())
  )
  expect_s3_class(
    res[[1]],
    "data.frame"
  )
  expect_gt(
    nrow(res[[1]]),
    200
  )
})
