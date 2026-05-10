#' Get VPTS data from the public BirdCast NEXRAD archive
#'
#' Gets VPTS data from the public BirdCast NEXRAD archive.
#'
#' @details
#' By default, data are retrieved from the public BirdCast S3 archive at
#' `https://birdcastdata.s3.amazonaws.com/nexrad/daily`.
#'
#' The expected path format is:
#' `"{radar}/{year}/{radar}_vpts_{year}{month}{day}.csv"`.
#'
#' @section Inner working:
#' - Checks that the requested radar is present in the NEXRAD coverage file.
#' - Checks that data exist for the requested radar/date combination.
#' - Constructs the S3 paths for the daily VPTS files from the coverage file.
#' - Performs parallel HTTP requests to fetch the VPTS CSV data.
#' - Parses the response bodies with the shared VPTS column classes.
#' - Uses uppercase NEXRAD radar codes for archive paths.
#' - Adds a column with the radar source.
#'
#' @param radar NEXRAD radar code.
#' @param rounded_interval Interval to fetch data for, rounded to nearest day.
#' @param coverage A data frame containing the coverage of the BirdCast NEXRAD
#'   archive. If not provided, it will be fetched via the internet.
#' @return A tibble with VPTS data.
#' @keywords internal
get_vpts_nexrad <- function(
  radar,
  rounded_interval,
  coverage = get_vpts_coverage_nexrad()
) {
  radar <- toupper(radar)

  # Check that only one radar is provided.
  check_odim_nexrad_scalar(radar)

  # Check if the requested radar is present in the coverage.
  if (!all(radar %in% coverage$radar)) {
    missing_radar <- radar[!radar %in% coverage$radar]

    cli::cli_abort(
      "Can't find radar {.val {missing_radar}} in the NEXRAD coverage file
       (see {.fun get_vpts_coverage}).",
      missing_radar = missing_radar,
      class = "getRad_error_nexrad_radar_not_found"
    )
  }

  # Check if the requested radar/date combination is present in the coverage.
  filtered_coverage <- dplyr::filter(
    coverage,
    .data$radar %in% radar,
    .data$date %within% rounded_interval
  )

  if (nrow(filtered_coverage) == 0) {
    cli::cli_abort(
      "Can't find any data for the requested radar(s) and date(s).",
      class = "getRad_error_date_not_found"
    )
  }

  # Convert the selected coverage rows into paths on the BirdCast NEXRAD archive.
  s3_paths <- filtered_coverage |>
    dplyr::mutate(
      path = glue::glue(
        "{radar}/{year}/{radar}_vpts_{year}{month}{day}.csv",
        radar = .data$radar,
        year = lubridate::year(.data$date),
        month = sprintf("%02d", lubridate::month(.data$date)),
        day = sprintf("%02d", lubridate::day(.data$date))
      )
    ) |>
    dplyr::pull(.data$path)

  # Read the VPTS CSV files.
  nexrad_data_url <- "https://birdcastdata.s3.amazonaws.com/nexrad/daily"
  radar_out <- tolower(radar)

  paste(nexrad_data_url, s3_paths, sep = "/") |>
    read_vpts_from_url() |>
    purrr::keep(.p = ~ as.logical(nrow(.x))) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      radar = .env$radar_out,
      source = "nexrad"
    )
}