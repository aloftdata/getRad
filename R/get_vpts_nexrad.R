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
#' - Constructs the S3 paths for the daily VPTS files based on the input.
#' - Performs parallel HTTP requests to fetch the VPTS CSV data.
#' - Parses the response bodies with the shared VPTS column classes.
#' - Uses uppercase NEXRAD radar codes for archive paths.
#' - Adds a column with the radar source.
#'
#' @param radar NEXRAD radar code.
#' @param rounded_interval Interval to fetch data for, rounded to nearest day.
#' @return A tibble with VPTS data.
#' @keywords internal
get_vpts_nexrad <- function(radar, rounded_interval) {
  radar <- toupper(radar)

  dates <- seq(
    lubridate::as_date(lubridate::int_start(rounded_interval)),
    lubridate::as_date(lubridate::int_end(rounded_interval)),
    by = "day"
  )

  s3_paths <- purrr::map_chr(
    dates,
    \(date) {
      glue::glue(
        "{radar}/{year}/{radar}_vpts_{year}{month}{day}.csv",
        radar = radar,
        year = lubridate::year(date),
        month = sprintf("%02d", lubridate::month(date)),
        day = sprintf("%02d", lubridate::day(date))
      )
    }
  )

  base_url <- "https://birdcastdata.s3.amazonaws.com/nexrad/daily"

  out <-
    paste(base_url, s3_paths, sep = "/") |>
    read_vpts_from_url() |>
    purrr::keep(.p = ~ as.logical(nrow(.x))) |>
    purrr::list_rbind()

  if (nrow(out) == 0) {
    return(tibble::tibble())
  }

  out |>
    dplyr::mutate(
      radar = tolower(.data$radar),
      source = "nexrad"
    )
}