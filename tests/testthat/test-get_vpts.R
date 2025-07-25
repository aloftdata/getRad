test_that("get_vpts() can return vpts data as a tibble or vpts object", {
  skip_if_offline()
  # Test that the function can return data as a vpts object
  ## Create vpts object to test on, but only if it doesn't exist. This way the
  ## tests can run in any order.
  returned_vpts_object <-
    get0(
      "returned_vpts_object",
      ifnotfound = get_vpts(
        radar = "depro",
        datetime = "2016-03-05",
        source = "uva",
        return_type = "vpts"
      )
    )

  returned_vpts_object_default <-
    get0(
      "returned_vpts_object_default",
      ifnotfound = get_vpts(
        radar = "depro",
        datetime = "2016-03-05",
        source = "uva"
      )
    )
  expect_s3_class(
    returned_vpts_object,
    "vpts"
  )
  expect_type(
    returned_vpts_object,
    "list"
  )
  expect_s3_class(
    returned_vpts_object_default,
    "vpts"
  )
  expect_type(
    returned_vpts_object_default,
    "list"
  )

  # Skip if the RMI can not be reached.
  skip_if_offline("opendata.meteo.be")
  returned_vpts_object_rmi <-
    get0(
      "returned_vpts_object",
      ifnotfound = get_vpts(
        radar = "bejab",
        datetime = "2020-01-19",
        source = "rmi",
        return_type = "vpts"
      )
    )

  expect_s3_class(
    returned_vpts_object_rmi,
    "vpts"
  )
  expect_type(
    returned_vpts_object_rmi,
    "list"
  )

  # Test that the function can return data as a tibble
  expect_s3_class(
    get_vpts(
      radar = "bejab",
      datetime = "2023-01-01",
      source = "baltrad",
      return_type = "tibble"
    ),
    "data.frame"
  )
})

test_that("get_vpts() returns a vpts object by default", {
  skip_if_offline()

  ## Create vpts object to test on, but only if it doesn't exist. This way the
  ## tests can run in any order.
  returned_vpts_object <-
    get0(
      "returned_vpts_object",
      ifnotfound = get_vpts(
        radar = "depro",
        datetime = "2016-03-05",
        source = "uva",
        return_type = "vpts"
      )
    )

  expect_identical(
    returned_vpts_object,
    get_vpts(
      radar = "depro",
      datetime = "2016-03-05",
      source = "uva"
    )
  )
})

test_that("get_vpts() can fetch vpts data for a single radar and time", {
  skip_if_offline()
  single_radar_single_day <-
    get_vpts(
      radar = "bejab",
      datetime = "2023-01-01",
      source = "baltrad",
      return_type = "tibble"
    )
  expect_s3_class(
    # A known radar and date combination that ALOFT has data for
    single_radar_single_day,
    "data.frame"
  )

  # Expect a known length so no rows have been accidentally filtered away
  expect_identical(
    nrow(single_radar_single_day),
    7125L
  )
})

test_that("get_vpts() can fetch vpts data for multiple radars", {
  skip_if_offline()
  multiple_radars <- get_vpts(
    radar = c("bejab", "bewid"),
    datetime = "2023-01-01",
    source = "baltrad",
    return_type = "tibble"
  )
  expect_s3_class(
    multiple_radars,
    "data.frame"
  )

  expect_contains(
    multiple_radars$radar,
    c("bejab", "bewid")
  )
})

test_that("get_vpts() can fetch data from a single radar source", {
  skip_if_offline()
  # Only data from UVA available for this radar day
  expect_identical(
    get_vpts(
      radar = "bejab",
      datetime = "2018-02-02",
      source = "uva",
      return_type = "tibble"
    )$source |>
      unique(),
    "uva"
  )
  # radar day has data both on UVA and BALTRAD
  expect_identical(
    get_vpts(
      radar = "bejab",
      datetime = "2018-05-18",
      source = "baltrad",
      return_type = "tibble"
    )$source |>
      unique(),
    "baltrad"
  )

  expect_identical(
    get_vpts(
      radar = "bejab",
      datetime = "2018-05-18",
      source = "uva",
      return_type = "tibble"
    )$source |>
      unique(),
    "uva"
  )
})

test_that("get_vpts() returns a single vpts object per radar", {
  expect_length(
    get_vpts(
      radar = c("bejab", "nldhl"),
      datetime = "2023-01-01",
      source = "baltrad"
    ),
    2L
  )
})

test_that("get_vpts() returns columns of the expected type and order", {
  skip_if_offline()

  # A helper in bioRad (validate_vpts()) that we call indirectly via
  # bioRad::as.vpts() currently doesn't support factors: bioRad v0.8.1
  expected_col_types <-
    list(
      source = "character",
      radar = "character",
      datetime = c("POSIXct", "POSIXt"),
      height = "integer",
      u = "numeric",
      v = "numeric",
      w = "numeric",
      ff = "numeric",
      dd = "numeric",
      sd_vvp = "numeric",
      gap = "logical",
      eta = "numeric",
      dens = "numeric",
      dbz = "numeric",
      dbz_all = "numeric",
      n = "integer",
      n_dbz = "integer",
      n_all = "integer",
      n_dbz_all = "integer",
      rcs = "numeric",
      sd_vvp_threshold = "numeric",
      vcp = "integer",
      radar_latitude = "numeric",
      radar_longitude = "numeric",
      radar_height = "integer",
      radar_wavelength = "numeric",
      source_file = "character"
    )

  expect_identical(
    get_vpts(
      radar = c("deflg"),
      datetime = lubridate::ymd("20171015"),
      source = "baltrad",
      return_type = "tibble"
    ) |>
      purrr::map(class),
    expected_col_types
  )

  # Specific radar causing trouble
  expect_identical(
    get_vpts(
      radar = c("dehnr"),
      datetime = lubridate::ymd("20171015"),
      source = "uva",
      return_type = "tibble"
    ) |>
      purrr::map(class),
    expected_col_types
  )
})

test_that("get_vpts() can fetch data from a specific source only", {
  skip_if_offline()

  # Data from only BALTRAD even if UVA is available for the same interval
  expect_identical(
    get_vpts(
      radar = "bejab",
      datetime = "2018-05-18",
      source = "baltrad",
      return_type = "tibble"
    ) |>
      dplyr::pull("source") |>
      unique(),
    "baltrad"
  )
})

test_that("get_vpts() can fetch vpts data for a date range", {
  skip_if_offline()

  radar_interval <- get_vpts(
    radar = "bejab",
    lubridate::interval(
      lubridate::ymd("2023-01-01"),
      lubridate::ymd("2023-01-02")
    ),
    source = "baltrad",
    return_type = "tibble"
  )
  expect_s3_class(
    radar_interval,
    "data.frame"
  )

  # Check that the requested dates are present in the output
  expect_in(
    unique(as.Date((radar_interval$datetime))),
    c(as.Date("2023-01-01"), as.Date("2023-01-02"))
  )
})

test_that("get_vpts() returns data for a whole day if datetime has no time", {
  skip_if_offline()
  single_radar_single_day <-
    get_vpts(
      radar = "bejab",
      datetime = "2023-01-01",
      source = "baltrad",
      return_type = "tibble"
    )

  days_returned <-
    dplyr::pull(single_radar_single_day, dplyr::all_of("datetime")) |>
    lubridate::floor_date(unit = "days") |>
    unique()

  expect_length(
    days_returned,
    1L
  )
})

test_that("get_vpts() returns data for the interval provided only", {
  skip_if_offline()
  ## Define an interval to fetch data for
  int_start <- lubridate::ymd_hms("2024-10-21 14:22:11")
  int_end <- lubridate::ymd_hms("2024-10-21 15:23:07")

  radar_interval_hhmmss <- get_vpts(
    radar = "seoer",
    lubridate::interval(int_start, int_end),
    source = "baltrad",
    return_type = "tibble"
  )

  ## The function should always round the timestamps down, this causes the least
  ## surprise
  ### get the measuring frequency (select the most common one)
  update_freq <-
    dplyr::distinct(radar_interval_hhmmss, datetime) |>
    dplyr::pull(dplyr::all_of("datetime")) |>
    diff() |>
    mean() |>
    as.numeric(units = "mins")

  expect_identical(
    min(radar_interval_hhmmss$datetime),
    lubridate::ceiling_date(int_start, unit = paste(update_freq, "minutes"))
  )

  expect_identical(
    max(radar_interval_hhmmss$datetime),
    lubridate::floor_date(int_end, unit = paste(update_freq, "minutes"))
  )
})

test_that("get_vpts() supports POSIXct dates", {
  skip_if_offline()

  radar_interval <- get_vpts(
    radar = "nlhrw",
    datetime = as.POSIXct("2025-05-07 14:53:37", tz = "Europe/Berlin"),
    return_type = "tibble"
  )

  expect_s3_class(
    radar_interval,
    "data.frame"
  )

  # Check that the requested dates are present in the output
  expect_in(
    unique(as.Date((radar_interval$datetime))),
    as.Date("2025-05-07")
  )
})

test_that("get_vpts() only returns the data for the requested day", {
  # Check bug where one extra day of data was returned due to rounding error

  radar_interval <- get_vpts(
    radar = "nlhrw",
    datetime = t <- as.POSIXct("2025-04-10 13:45:04", tz = "Europe/Berlin"),
    return_type = "tibble"
  )

  expect_equal(
    unique(radar_interval$datetime),
    lubridate::with_tz(t, "UTC")
  )
})

test_that("get_vpts() supports date intervals with hours and minutes", {
  skip_if_offline()

  hour_minute_interval <-
    get_vpts(
      radar = "bejab",
      lubridate::interval(
        lubridate::ymd_hms("2023-01-01 12:00:00"),
        lubridate::ymd_hms("2023-01-01 16:59:59")
      ),
      source = "baltrad",
      return_type = "tibble"
    )

  expect_s3_class(
    hour_minute_interval,
    "data.frame"
  )

  # The minimum returned date should be within the interval
  expect_gte(
    min(hour_minute_interval$datetime),
    lubridate::ymd_hms("2023-01-01 12:00:00")
  )
  # The maximum returned datetime should be within the interval
  expect_lte(
    max(hour_minute_interval$datetime),
    lubridate::ymd_hms("2023-01-01 16:59:59")
  )

  # The maximum should actually be rounded by a 15 minute interval
  expect_identical(
    max(hour_minute_interval$datetime),
    lubridate::ymd_hms("2023-01-01 16:45:00")
  )
})

test_that("get_vpts() can return data as a vpts object compatible with getRad", {
  skip_if_offline()
  skip_on_os("windows") # Does this test cause the Windows CI to timeout?

  ## Create vpts object to test on, but only if it doesn't exist. This way the
  ## tests can run in any order.
  returned_vpts_object <-
    get0(
      "returned_vpts_object",
      ifnotfound = get_vpts(
        radar = "depro",
        datetime = "2016-03-05",
        source = "uva",
        return_type = "vpts"
      )
    )

  # Array of combinations that exposed bugs in the past
  expect_identical(
    list(
      radar = list("bejab", "depro", "bejab", "bejab"),
      datetime = list(
        "2018-05-18",
        "2016-03-05",
        "2018-05-31",
        lubridate::interval("2018-05-31 18:00:00", "2018-06-01 02:00:00")
      ),
      source = list("baltrad", "uva", "baltrad", "baltrad")
    ) |>
      purrr::pmap(get_vpts) |>
      purrr::map_chr(class),
    c("vpts", "vpts", "vpts", "vpts")
  )

  # The returned object should be identical as if created via bioRad
  expect_identical(
    get_vpts(
      radar = "depro",
      datetime = "2016-03-05",
      source = "uva",
      return_type = "tibble"
    ) |>
      dplyr::select(-source) |>
      bioRad::as.vpts(),
    returned_vpts_object
  )
  # This also works when multiple radars are selected
  ## In this case a list of vpts objects should be returned
  expect_type(
    get_vpts(
      radar = c("depro", "bejab"),
      datetime = "2016-03-05",
      source = "uva"
    ),
    "list"
  )

  expect_identical(
    get_vpts(
      radar = c("depro", "bejab"),
      datetime = "2016-03-05",
      source = "uva"
    ) |>
      purrr::map_chr(class) |>
      unname(), # only test for class, names are tested elsewhere
    c("vpts", "vpts")
  )

  ## This list should be named the same as the requested radars
  requested_radars <- c("depro", "bejab")
  expect_named(
    get_vpts(
      radar = c("depro", "bejab"),
      datetime = "2016-03-05",
      source = "uva",
      return_type = "vpts"
    ),
    requested_radars
  )

  ## The named child objects should correspond to the correct vpts objects (bug)
  expect_identical(
    get_vpts(
      radar = c("depro", "bejab"),
      datetime = "2016-03-05",
      source = "uva",
      return_type = "vpts"
    ) |>
      purrr::chuck("bejab"),
    get_vpts(
      radar = "bejab",
      datetime = "2016-03-05",
      source = "uva",
      return_type = "vpts"
    )
  )

  # Radar day where vpts objects on aloft have 3 different values for the radar
  # column, script should have all convert them to the same odim value
  expect_identical(
    purrr::chuck(get_vpts("bejab", "2018-05-18", "baltrad"), "radar"),
    "bejab"
  )
  expect_identical(
    get_vpts("bejab", "2018-05-18", "baltrad", return_type = "tibble") |>
      dplyr::pull(.data$radar) |>
      unique(),
    "bejab"
  )
})

test_that("get_vpts() returns an error when multiple sources are provided", {
  skip_if_offline()

  expect_error(
    get_vpts(
      radar = "bejab",
      datetime = "2018-05-18",
      source = c("baltrad", "uva")
    ),
    class = "getRad_error_multiple_sources"
  )
})

test_that("get_vpts() returns an error when an invalid source is provided", {
  expect_error(
    get_vpts("bejab", "20241118", "not a source"),
    class = "getRad_error_source_invalid"
  )

  expect_error(
    get_vpts("bejab", "20241118", "baltradz"),
    class = "getRad_error_source_invalid"
  )
})

test_that("get_vpts() uses baltrad if no source is provided", {
  expect_identical(
    unique(get_vpts("bejab", "20180525", return_type = "tibble")$source),
    "baltrad"
  )
})

test_that("get_vpts() returns an error if NULL is passed as a source", {
  expect_error(
    get_vpts("bejab", "20180525", source = NULL),
    class = "getRad_error_source_missing"
  )
})

test_that("get_vpts() returns an error for a bad radar", {
  skip_if_offline()

  # Radar not found in ALOFT coverage
  expect_error(
    get_vpts(
      radar = "aaaaa",
      datetime = "2023-01-01",
      source = "uva"
    ),
    class = "getRad_error_aloft_radar_not_found"
  )

  # Radar is not a character vector
  expect_error(
    get_vpts(radar = 1:3, datetime = "2023-01-01", source = "uva"),
    class = "getRad_error_radar_not_character"
  )
})

test_that("get_vpts() returns an error for a bad time argument", {
  skip_if_offline()

  # Date not found in ALOFT coverage
  expect_error(
    get_vpts(radar = "bejab", datetime = "9000-01-02", source = "baltrad"),
    class = "getRad_error_date_not_found"
  )
  # Time is not parsable to a date or interval
  expect_error(
    get_vpts(radar = "bejab", datetime = 1:3, source = "baltrad"),
    class = "getRad_error_date_parsable"
  )
})
test_that("`get_vpts` is tz insensitive", {
  t <- as.POSIXct("2025-1-3 1:00:00", tz = "CET")
  tUtc <- lubridate::with_tz(t, "UTC")
  expect_identical(
    get_vpts("nlhrw", lubridate::as.interval(t, t + lubridate::minutes(30))),
    get_vpts("nlhrw", lubridate::as.interval(tUtc, tUtc + lubridate::minutes(30)))
  )
})

test_that("`get_vpts` for a single time gets closest nominal time", {
  t <- as.POSIXct("2025-1-3 1:00:00", tz = "CET")
  expect_identical(
    get_vpts("nlhrw", t),
    get_vpts("nlhrw", t + lubridate::seconds(123))
  )
  expect_identical(
    get_vpts("nlhrw", t + lubridate::seconds(4)),
    get_vpts("nlhrw", t + lubridate::seconds(123))
  )
})
