#' Get VPTS data from the Aloft bucket
#'
#' Gets VPTS data from the Aloft bucket.
#'
#' @details
#' ```{r aloft_data_url, echo = FALSE, results = FALSE}
#' data_url <- getOption("getRad.aloft_data_url")
#' ```
#'
#' By default, data from the [Aloft bucket](https://aloftdata.eu/browse/) are
#' retrieved from <`r data_url`>. This can be changed by
#' setting `options(getRad.aloft_data_url)` to any desired url.
#'
#' @section Inner working:
#' - Constructs the S3 paths for the VPTS files based on the input.
#' - Performs parallel HTTP requests to fetch the VPTS CSV data.
#' - Parses the response bodies with some assumptions about the column classes.
#' - Adds a column with the radar source.
#' - Overwrites the radar column with the radar_odim_code, all other values for
#' this column are considered in error.
#'
#' @param radar_odim_code Radar ODIM code.
#' @param rounded_interval Interval to fetch data for, rounded to nearest day.
#' @param source Source of the data. One of `baltrad`, `uva` or `ecog-04003`.
#' @param coverage A data frame containing the coverage of the Aloft bucket.
#'   If not provided, it will be fetched from via the internet.
#' @return A tibble with VPTS data.
#' @keywords internal
#' @examplesIf interactive()
#' get_vpts_aloft(
#'   radar_odim_code = "bejab",
#'   rounded_interval = lubridate::interval("20240305", "20240307"),
#'   source = "baltrad"
#' )
get_vpts_aloft <- function(radar_odim_code,
                           rounded_interval,
                           source = c("baltrad", "uva", "ecog-04003"),
                           coverage = aloft_data_coverage()) {
  # rename source argument for readability
  selected_source <- source

  # Check that only one radar is provided (string of length 1)
  check_odim_scalar(radar_odim_code)

  # Check if the requested radars are present in the coverage
  if (!all(radar_odim_code %in% coverage$radar)) {
    missing_radar <- radar_odim_code[!radar_odim_code %in% coverage$radar]
    cli::cli_abort(
      "Radar not found in ALOFT coverage:
      {missing_radar}.",
      missing_radar = missing_radar,
      class = "getRad_error_aloft_radar_not_found"
    )
  }

  # Check if the requested date radar combination is present in the coverage
  filtered_coverage <- dplyr::filter(
    coverage,
    .data$radar %in% radar_odim_code,
    .data$date %within% rounded_interval,
    .data$source == selected_source
  )
  at_least_one_radar_date_combination_exists <- nrow(filtered_coverage) > 0

  if (!at_least_one_radar_date_combination_exists) {
    cli::cli_abort(
      "No data found for the requested radar(s) and date(s).",
      class = "getRad_error_date_not_found"
    )
  }

  # Check if the requested radars are present in the coverage for the source.
  found_radars <-
    dplyr::filter(
      coverage,
      .data$source %in% selected_source,
      .data$radar %in% radar_odim_code
    ) |>
    dplyr::pull("radar")
  missing_radars <- setdiff(radar_odim_code, found_radars)

  if (!all(radar_odim_code %in% coverage$radar)) {
    cli::cli_abort(
      "{length(missing_radars)} Radar{?s} not found in {source} coverage:
      {glue::backtick(missing_radars)}",
      missing_radars = missing_radars,
      class = "getRad_error_radar_not_found"
    )
  }

  # Filter the coverage data to the selected radars and time interval and
  # convert into paths on the aloft s3 storage
  ## We need to use the rounded interval because coverage only has daily
  ## resolution
  s3_paths <- filtered_coverage |>
    dplyr::pull(.data$directory) |>
    # Replace hdf5 with daily to fetch vpts files instead of hdf5 files
    string_replace("hdf5", "daily") |>
    # Construct the filename using glue mapping over every path.
    purrr::map_chr(\(path) {
      glue::glue(
        "{dir}/{radar}_vpts_{year}{month}{day}.csv",
        dir = string_extract(path, ".+/.+/.+/[0-9]{4}"),
        radar = string_extract(path, "(?<=daily/).{5}"),
        year = string_extract(path, "(?<=\\/)[0-9]{4}"),
        month = string_extract(path, "(?<=/)[0-9]{2}(?=/)"),
        day = string_extract(path, "[0-9]{2}$")
      )
    })

  # Read the vpts csv files
  aloft_data_url <- getOption("getRad.aloft_data_url")

  paste(aloft_data_url, s3_paths, sep = "/") |>
    read_vpts_from_url() |>
    # Add a column with the radar source to not lose this information
    purrr::map2(
      s3_paths,
      ~ dplyr::mutate(.x,
        source = string_extract(
          .y,
          ".+(?=\\/daily)"
        )
      )
    ) |>
    purrr::list_rbind() |>
    # Move the source column to the front, where it makes sense
    dplyr::relocate(dplyr::all_of("source"), .before = dplyr::all_of("radar")) |>
    # Overwrite the radar column with the radar_odim_date, all other values are
    # considered invalid for aloft
    dplyr::mutate(radar = radar_odim_code)
}
