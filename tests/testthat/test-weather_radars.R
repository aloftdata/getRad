test_that("weather_radars source `argument`", {
  expect_error(weather_radars(1),
    class = "getRad_error_weather_radar_source_not_character"
  )
  expect_error(weather_radars(character()),
    class = "getRad_error_weather_radar_source_not_character"
  )
  expect_error(weather_radars(c("asdf", NA)),
    class = "getRad_error_weather_radar_source_not_character"
  )
  expect_error(weather_radars(c("opera", "nextrad")),
    class = "getRad_error_weather_radar_source_not_valid"
  )
})
test_that("weather_radars returns a tibble", {
  skip_if_offline(host = "eumetnet.eu")
  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- weather_radars()
  }

  expect_s3_class(weather_radar_metadata, "tbl_df")
})

test_that("weather_radars returns non-empty tibble", {
  skip_if_offline(host = "eumetnet.eu")
  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- weather_radars()
  }

  expect_true(nrow(weather_radar_metadata) > 0, "Expected non-empty tibble")
})

test_that("weather_radars returns a tibble with expected columns", {
  skip_if_offline(host = "eumetnet.eu")
  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- weather_radars()
  }

  ## Right number of columns
  expect_length(
    weather_radar_metadata,
    31
  )

  ## Right columns, in a certain order
  expect_named(
    ignore.order = FALSE,
    weather_radar_metadata,
    c(
      "radar",
      "number",
      "country",
      "countryid",
      "oldcountryid",
      "wmocode",
      "wigosid",
      "odimcode",
      "location",
      "status",
      "latitude",
      "longitude",
      "heightofstation",
      "band",
      "doppler",
      "polarization",
      "maxrange",
      "startyear",
      "heightantenna",
      "diameterantenna",
      "beam",
      "gain",
      "frequency",
      "stratus",
      "cirusnimbus",
      "wrwp",
      "finishyear",
      "singlerrr",
      "compositerrr",
      "origin",
      "source"
    )
  )
})

test_that("weather_radars returns tibble with correct data types", {
  skip_if_offline(host = "eumetnet.eu")
  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- weather_radars()
  }

  expect_identical(
    purrr::map(weather_radar_metadata, class),
    list(
      radar = "character",
      number = "integer",
      country = "character",
      countryid = "character",
      oldcountryid = "character",
      wmocode = "integer",
      wigosid = "character",
      odimcode = "character",
      location = "character",
      status = "integer",
      latitude = "numeric",
      longitude = "numeric",
      heightofstation = "integer",
      band = "character",
      doppler = "logical",
      polarization = "character",
      maxrange = "integer",
      startyear = "integer",
      heightantenna = "numeric",
      diameterantenna = "numeric",
      beam = "numeric",
      gain = "numeric",
      frequency = "numeric",
      stratus = "character", # currently unused?
      cirusnimbus = "character", # currently unused?
      wrwp = "logical",
      finishyear = "integer",
      singlerrr = "logical",
      compositerrr = "logical",
      origin = "character",
      source = "character"
    )
  )
})

test_that("weather_radars() should return a table with records from main and archive", {
  skip_if_offline(host = "eumetnet.eu")

  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- weather_radars()
  }

  ## Count the number of records in both main and archive source
  n_records_main <-
    paste(
      sep = "/",
      "http://eumetnet.eu/wp-content/themes/aeron-child",
      "observations-programme/current-activities/opera/database",
      "OPERA_Database/OPERA_RADARS_DB.json"
    ) |>
    httr2::request() |>
    req_user_agent_getrad() |>
    req_retry_getrad() |>
    httr2::req_perform() |>
    # The object is actually returned as text/plain
    httr2::resp_body_json(check_type = FALSE) |>
    length()

  n_records_archive <-
    paste(
      sep = "/",
      "http://eumetnet.eu/wp-content/themes/aeron-child",
      "observations-programme/current-activities/opera/database",
      "OPERA_Database/OPERA_RADARS_ARH_DB.json"
    ) |>
    httr2::request() |>
    req_user_agent_getrad() |>
    req_retry_getrad() |>
    httr2::req_perform() |>
    # The object is actually returned as text/plain
    httr2::resp_body_json(check_type = FALSE) |>
    length()

  # Compare to output of weather_radars()
  expect_identical(
    nrow(weather_radar_metadata),
    n_records_main + n_records_archive
  )
})

test_that("weather_radars() should return a source column", {
  skip_if_offline(host = "eumetnet.eu")

  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- weather_radars()
  }

  ## Is the source column present?
  expect_true(
    "source" %in% names(weather_radar_metadata)
  )

  ## Does it contain only the values `main` and `archive`?
  expect_in(
    weather_radar_metadata$origin,
    c("main", "archive")
  )
})
test_that("weather_radars() doesn't return empty strings, but NA instead", {
  skip_if_offline(host = "eumetnet.eu")

  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- weather_radars()
  }

  ## Are there any empty strings in the tibble?
  ### Character columns
  expect_false(
    weather_radar_metadata |>
      dplyr::summarise(
        dplyr::across(
          dplyr::where(is.character),
          \(x) any(x == "", na.rm = TRUE)
        )
      ) |>
      any()
  )

  ### Fail on the first character column that contains an empty string
  weather_radar_metadata |>
    dplyr::summarise(
      dplyr::across(
        dplyr::where(is.character),
        \(x) any(x == "", na.rm = TRUE)
      )
    ) |>
    purrr::walk(expect_false)


  ### All columns
  expect_false(
    any(
      sapply(
        weather_radar_metadata,
        function(x) any(x == "", na.rm = TRUE)
      )
    )
  )
})
test_that("weather_radars nexrad downloads", {
  skip_if_offline(host = "ncei.noaa.gov")

  expect_named(
    weather_radars(
      "nexrad",
      c(
        "ncdcid", "icao", "wban", "name", "country", "st", "county",
        "elev", "utc", "stntype", "radar", "latitude", "longitude",
        "location", "heightantenna", "source"
      )
    )
  )
  expect_gt(nrow(weather_radars("nexrad")), 170)
})

test_that("weather_radars nexrad downloads", {
  skip_if_offline(host = "ncei.noaa.gov")
  skip_if_offline(host = "eumetnet.eu")

  expect_identical(weather_radars(c("opera", "nexrad")), weather_radars("all"))
})
