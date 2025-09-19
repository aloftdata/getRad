test_that("get_weather_radars source `argument`", {
  expect_error(get_weather_radars(1),
    class = "getRad_error_weather_radar_source_not_character"
  )
  expect_error(get_weather_radars(character()),
    class = "getRad_error_weather_radar_source_not_character"
  )
  expect_error(get_weather_radars(c("asdf", NA)),
    class = "getRad_error_weather_radar_source_not_character"
  )
  expect_error(
    get_weather_radars(c("opera", "nextrad")),
    "must be one of"
  )
})

test_that("get_weather_radars returns a sf by default", {
  skip_if_offline(host = "eumetnet.eu")
  expect_s3_class(get_weather_radars(), "sf")
})

test_that("get_weather_radars returns non-empty tibble", {
  skip_if_offline(host = "eumetnet.eu")
  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- get_weather_radars()
  }

  expect_true(nrow(weather_radar_metadata) > 0, "Expected non-empty sf")
})

test_that("get_weather_radars returns a tibble with expected columns", {
  skip_if_offline(host = "eumetnet.eu")
  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- get_weather_radars()
  }

  ## Right number of columns
  expect_length(
    weather_radar_metadata,
    32
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
      "source",
      "geometry"
    )
  )
})

test_that("get_weather_radars returns sf with correct data types", {
  skip_if_offline(host = "eumetnet.eu")
  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- get_weather_radars()
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
      source = "character",
      geometry = c("sfc_POINT", "sfc")
    )
  )
})

test_that("get_weather_radars() should return a table with records from main and archive", {
  skip_if_offline(host = "eumetnet.eu")

  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- get_weather_radars()
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

test_that("get_weather_radars() should return a origin column", {
  skip_if_offline(host = "eumetnet.eu")

  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- get_weather_radars()
  }

  ## Is the origin column present?
  expect_true(
    "origin" %in% names(weather_radar_metadata)
  )

  ## Does it contain only the values `main` and `archive`?
  expect_in(
    weather_radar_metadata$origin,
    c("main", "archive")
  )
})
test_that("get_weather_radars() doesn't return empty strings, but NA instead", {
  skip_if_offline(host = "eumetnet.eu")

  if (!exists("weather_radar_metadata")) {
    weather_radar_metadata <- get_weather_radars()
  }

  ## Are there any empty strings in the tibble?
  ### Character columns
  expect_false(
    weather_radar_metadata |>
      sf::st_drop_geometry() |>
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
    sf::st_drop_geometry() |>
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
        sf::st_drop_geometry(weather_radar_metadata),
        function(x) any(x == "", na.rm = TRUE)
      )
    )
  )
})
test_that("get_weather_radars nexrad downloads", {
  skip_if_offline(host = "ncei.noaa.gov")

  expect_named(
    get_weather_radars(
      "nexrad"
    ),
    c(
      "radar", "ncdcid", "icao", "wban", "name", "country", "st",
      "county", "elev", "utc", "stntype", "latitude", "longitude",
      "location", "heightantenna", "source", "geometry"
    )
  )
  expect_gt(nrow(get_weather_radars("nexrad", return_type = "tibble")), 170)
})



test_that("get_weather_radars is the same for both return types (besides geometry)", {
  skip_if_offline(host = "ncei.noaa.gov")
  skip_if_offline(host = "eumetnet.eu")
  expect_identical(
    get_weather_radars(c("opera", "nexrad")),
    get_weather_radars("all")
  )
})
