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
