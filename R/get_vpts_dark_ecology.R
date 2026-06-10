#' Read dark ecology vpts profiles from disk
#'
#' @param path the directory where to search fir files
#' @param radar the radars to search for
#' @param rounded_interval the time range to search in
#' @param ... additional arguments passed to `bioRad::read_cajun()`
#' @param call the call for error messages
#'
#' @returns a vpts for now
#' @noRd
#' @examples
#' get_vpts_local_dark_ecology(
#'   radar = "KCBX",
#'   rounded_interval = lubridate::interval(start = "20150101", end = "20150201")
#' )
get_vpts_local_dark_ecology <- function(
  path,
  radar,
  rounded_interval,
  ...,
  call = rlang::caller_env()
) {
  dates <- as.Date(seq(
    lubridate::int_start(rounded_interval),
    lubridate::int_end(rounded_interval),
    by = "day"
  ))
  sub_search_paths <- purrr::map(
    radar,
    ~ unique(glue::glue(
      getOption(
        "getRad.vpts_local_path_format_dark_ecology",
        default = "{year}/{month}/{day}/{radar}/"
      ),
      radar = .x,
      year = lubridate::year(dates),
      month = sprintf("%02i", lubridate::month(dates)),
      day = sprintf("%02i", lubridate::day(dates)),
      date = dates
    ))
  )
  search_paths <- purrr::map(sub_search_paths, ~ file.path(path, .x))
  # search_paths <-
  #   file.path(
  #     path,
  #     lubridate::year(days),
  #     string_pad(
  #       lubridate::month(days),
  #       width = 2,
  #       pad = "0",
  #       side = "left"
  #     ),
  #     string_pad(
  #       lubridate::day(days),
  #       width = 2,
  #       pad = "0",
  #       side = "left"
  #     ),
  #     radar
  #   )
  files <- purrr::map(
    search_paths,
    ~ .x |>
      purrr::keep(file.exists) |>
      purrr::map(.progress = "searching for local files", \(search_path) {
        # `{fs}` is much faster than base, and often already installed as it's a dependency of many tidyverse packages
        if (rlang::is_installed("fs")) {
          fs::dir_ls(
            path = search_path,
            recurse = TRUE,
            fail = FALSE
          )
        } else {
          list.files(
            path = search_path,
            recursive = TRUE,
            full.names = TRUE
          ) |>
            normalizePath()
        }
      }) |>
      unlist()
  )
  purrr::map(files, \(y) {
    bioRad::bind_into_vpts(purrr::map(y, bioRad::read_cajun, ...))
  })
}
