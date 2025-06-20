#' Get VPTS data from RMI
#'
#' Get VPTS data from [RMI_DATASET_CROW](
#' https://opendata.meteo.be/geonetwork/srv/eng/catalog.search#/metadata/RMI_DATASET_CROW).
#'
#' @inheritParams get_vpts_aloft
#' @return A tibble with VPTS data.
#' @keywords internal
#' @examplesIf interactive()
#' get_vpts_rmi(
#'   radar_odim_code = "bejab",
#'   rounded_interval = lubridate::interval("20200119", "20200124")
#' )
get_vpts_rmi <- function(radar_odim_code,
                         rounded_interval) {
  # Check the coverage for data availability
  coverage <- rmi_data_coverage(
    radar = radar_odim_code,
    year = seq(
      lubridate::year(lubridate::int_start(rounded_interval)),
      lubridate::year(lubridate::int_end(rounded_interval))
    )
  )

  # Check if the requested date radar combination is present in the coverage
  filtered_coverage <- dplyr::filter(
    coverage,
    .data$radar %in% radar_odim_code,
    .data$date %within% rounded_interval
  )
  at_least_one_radar_date_combination_exists <- nrow(filtered_coverage) > 0

  if (!at_least_one_radar_date_combination_exists) {
    cli::cli_abort(
      "No data found for the requested radar(s) and date(s).",
      class = "getRad_error_date_not_found"
    )
  }

  # Build the potential path for the rmi fwf files

  rmi_urls <- file.path(
    "https://opendata.meteo.be/ftp",
    filtered_coverage$directory,
    filtered_coverage$file
  )

  ## For every rmi url, parse the VPTS
  rmi_files <- read_lines_from_url(rmi_urls, parallel = FALSE)

  combined_vpts <-
    # drop the header for parsing
    purrr::map(rmi_files, \(lines) utils::tail(lines, -4)) |>
    purrr::map(parse_rmi) |>
    # Add the source_file column
    purrr::map2(rmi_files, ~ dplyr::mutate(.x,
      source_file =
        basename(get_rmi_sourcefile(.y))
    )) |>
    # Add the radar column from the file path
    purrr::map2(rmi_urls, ~ dplyr::mutate(.x,
      radar = string_extract(
        .y,
        "(?<=vbird\\/)[a-z]+"
      )
    )) |>
    purrr::list_rbind()

  # Enrich with metadata from `weather_radars()`, but only from the `main`
  # source to avoid duplicating rows
  radar_metadata <-
    weather_radars() |>
    dplyr::filter(.data$source == "main") |>
    dplyr::mutate(.data$odimcode,
      radar_latitude = .data$latitude,
      radar_longitude = .data$longitude,
      radar_height = .data$heightofstation,
      radar_wavelength = round(
        299792458 / (.data$frequency * 10^7), # speed of light in vacuum
        digits = 1
      ),
      .keep = "none"
    )

  enriched_vpts <-
    dplyr::left_join(combined_vpts,
      radar_metadata,
      by = dplyr::join_by("radar" == "odimcode")
    )

  enriched_vpts
}
