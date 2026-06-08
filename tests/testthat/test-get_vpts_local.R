test_that("correct error for no download function", {
  expect_error(
    get_vpts("KABX", as.Date('2323-1-5'), "birdcast", path = "./"),
    class = 'getRad_error_no_function_for_reading_local_source'
  )
})
test_that("correct error for non existing dir", {
  expect_error(
    get_vpts(
      "KABX",
      as.Date('2323-1-5'),
      "birdcast",
      path = file.path(tempdir(), "non_existing_path")
    ),
    class = 'getRad_error_path_not_a_dir'
  )
  withr::with_tempfile("file", fileext = ".csv", {
    write.csv(data.frame(a = 1:3), file)
    expect_true(file.exists(file))
    expect_error(
      get_vpts("KABX", as.Date('2323-1-5'), "birdcast", path = file),
      class = 'getRad_error_path_not_a_dir'
    )
  })
})
