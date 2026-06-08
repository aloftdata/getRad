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
  # Allow only one directory to be provided
  if (length(directory) > 1) {
    cli::cli_abort(
      "Only one directory can be provided, but {length(directory)} were given.",
      class = "dark_ecology_multiple_directories",
      call = call
    )
  }

  # Check that the provided directory exists
  if (!is_readable(directory)) {
    cli::cli_abort(
      "The provided directory does not exist or is not readable: {directory}",
      class = "dark_ecology_directory_not_found",
      call = call
    )
  }

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
