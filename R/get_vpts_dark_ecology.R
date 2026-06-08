#' Read dark ecology vpts profiles from disk
#'
#' @param directory
#' @param radar
#' @param rounded_interval
#' @param ... additional arguments passed to `bioRad::read_cajun()`
#' @param call
#'
#' @returns
#'
#' @examples
#' read_vpts_dark_ecology(
#'   radar = "KCBX",
#'   rounded_interval = lubridate::interval(start = "20150101", end = "20150201")
#' )
read_vpts_dark_ecology <- function(
  directory,
  radar,
  rounded_interval,
  ...
) {
  days <-
    seq(
      lubridate::int_start(rounded_interval),
      lubridate::int_end(rounded_interval),
      by = "day"
    )

  search_paths <-
    file.path(
      directory,
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
