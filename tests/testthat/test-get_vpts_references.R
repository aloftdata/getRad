test_that("add_references_vpts fails", {
  expect_error(
    add_reference_vpts(2, 1),
    class = "getRad_error_add_reference_vpts_invalid_source"
  )

  expect_error(
    add_reference_vpts(2, letters),
    class = "getRad_error_add_reference_vpts_invalid_source"
  )
  expect_error(
    add_reference_vpts(2, "nonsense_name"),
    class = "getRad_error_add_reference_vpts_invalid_source"
  )
  expect_error(
    add_reference_vpts(2, "baltrad"),
    class = "getRad_error_add_reference_vpts_only_df_and_vpts"
  )
})

test_that("Dark ecology reference", {
  dk_path <- system.file("extdata", "darkecology", package = "getRad")
  skip_if(
    dk_path == "",
    message = "No local data present for testing local read from dark ecology."
  )
  expect_s3_class(
    kcbx_vpts <- get_vpts(
      path = dk_path,
      radar = "KCBX",
      lubridate::interval(
        start = "20150101",
        end = "20150201"
      ),
      source = "dark_ecology"
    ),
    "vpts"
  )
  expect_identical(
    kcbx_vpts |> purrr::pluck("attributes", "references", 1),
    vptsReferences |> purrr::pluck("dark_ecology")
  )
  expect_s3_class(
    kcbx_vpts |> purrr::pluck("attributes", "references", 2),
    "bibentry"
  )
  expect_identical(
    kcbx_vpts |> purrr::pluck("attributes", "references", 2) |> attr("key"),
    "getRad"
  )
})
test_that("aloft reference", {
  expect_identical(
    get_vpts("bewid", as.Date("2023-2-1")) |>
      purrr::pluck("attributes", "references", 1),
    vptsReferences |> purrr::pluck("baltrad")
  )
  expect_identical(
    get_vpts("bewid", as.Date("2023-2-1"), source = "baltrad") |>
      purrr::pluck("attributes", "references", 1),
    vptsReferences |> purrr::pluck("baltrad")
  )
  expect_identical(
    get_vpts("nlhrw", as.Date("2023-2-1"), source = "uva") |>
      purrr::pluck("attributes", "references", 1),
    vptsReferences |> purrr::pluck("uva")
  )

  expect_identical(
    get_vpts("nldbl", as.Date("2016-10-6"), source = "ecog-04003") |>
      purrr::pluck("attributes", "references", 1),
    vptsReferences |> purrr::pluck("ecog-04003")
  )
  expect_identical(
    get_vpts(
      "nldbl",
      as.Date("2016-10-6"),
      source = "ecog-04003",
      return_type = "tibble"
    ) |>
      attr("references") |>
      purrr::pluck(1),
    vptsReferences |> purrr::pluck("ecog-04003")
  )
})


test_that("rmi reference", {
  expect_identical(
    get_vpts("bewid", as.Date("2023-2-1"), source = "rmi") |>
      purrr::pluck("attributes", "references", 1),
    vptsReferences |> purrr::pluck("rmi")
  )
})
test_that("birdcast reference", {
  expect_identical(
    get_vpts("KABX", as.Date("2023-2-1"), source = "birdcast") |>
      purrr::pluck("attributes", "references", 1),
    vptsReferences |> purrr::pluck("birdcast")
  )
})


test_that("references exist for all sources", {
  expect_identical(
    vptsReferences |> names() |> sort(),
    rlang::fn_fmls(get_vpts)$source |> eval() |> sort()
  )
  expect_s3_class(vptsReferences, "bibentry")
})
