#' Read dark ecology vpts profiles from disk via arrow
#'
#' @param path
#' @param radar
#' @param dateinterval
#'
#' @returns
#'
#' @examples
#' read_vpts_dark_ecology(
#'   radar = "KCBX",
#'   dateinterval = lubridate::interval(start = "20150101", end = "20150201")
#' )

read_vpts_dark_ecology <- function(
  path = "/media/pieter_huybrechts/data/profiles_lite/",
  radar,
  dateinterval
) {
  part <- arrow::DirectoryPartitioning$create(
    arrow::schema(
      year = arrow::int32(),
      month = arrow::int32(),
      day = arrow::int32(),
      station = arrow::utf8()
    )
  )

  profiles <- arrow::open_csv_dataset(path, partitioning = part) |>
    dplyr::mutate(
      filename = arrow::add_filename(),
      date = lubridate::ymd(paste(year, month, day))
    ) |>
    # required to create the filenames
    dplyr::collect() |>
    dplyr::mutate(
      datetime = lubridate::ymd_hms(paste(
        date,
        stringr::str_extract(filename, "[0-9]+(?=\\.)")
      ))
    ) |>
    dplyr::filter(.data$station == radar, .data$datetime %within% dateinterval)

  profiles
}

#' Read dark ecology vpts profiles from disk via vroom and guessing paths
#'
#' @param path
#' @param radar
#' @param dateinterval
#' @param ... additional arguments passed to `bioRad::read_cajun()`
#'
#' @returns
#'
#' @examples
#' read_vpts_dark_ecology2(
#'   radar = "KCBX",
#'   dateinterval = lubridate::interval(start = "20150101", end = "20150201")
#' )
read_vpts_dark_ecology2 <- function(
  path = "/media/pieter_huybrechts/data/profiles_lite/",
  radar,
  dateinterval,
  ...
) {
  days <-
    seq(
      lubridate::int_start(dateinterval),
      lubridate::int_end(dateinterval),
      by = "day"
    )

  search_paths <-
    file.path(
      path,
      lubridate::year(days),
      stringr::str_pad(
        lubridate::month(days),
        width = 2,
        pad = "0",
        side = "left"
      ),
      stringr::str_pad(
        lubridate::day(days),
        width = 2,
        pad = "0",
        side = "left"
      ),
      radar
    )

  search_paths |>
    purrr::keep(file.exists) |>
    purrr::map(.progress = "searching for local files", \(search_path) {
      fs::dir_ls(
        path = search_path,
        fail = FALSE
      )
    }) |>
    unlist() |>
    purrr::map(bioRad::read_cajun, ...)
}
