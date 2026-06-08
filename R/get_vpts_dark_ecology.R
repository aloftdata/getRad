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
#' get_vpts_dark_ecology(
#'   radar = "KCBX",
#'   rounded_interval = lubridate::interval(start = "20150101", end = "20150201")
#' )
get_vpts_dark_ecology <- function(
  directory,
  radar,
  rounded_interval,
  ...,
  call = rlang::caller_env()
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
      string_pad(
        lubridate::month(days),
        width = 2,
        pad = "0",
        side = "left"
      ),
      string_pad(
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
      # `{fs}` is much faster than base, and often already installed as it's a dependency of many tidyverse packages
      ifelse(
        rlang::is_installed("fs"),
        yes = {
          fs::dir_ls(
            path = search_path,
            recurse = TRUE,
            fail = FALSE
          )
        },
        no = {
          list.files(
            path = search_path,
            recursive = TRUE,
            full.names = TRUE
          ) |>
            normalizePath()
        }
      )
    }) |>
    unlist() |>
    purrr::map(bioRad::read_cajun, ...)
}
