#' Read dark ecology vpts profiles from disk
#'
#' @param path the directory where to search fir files
#' @param radar the radars to search for
#' @param rounded_interval the time range to search in
#' @param ... additional arguments passed to `bioRad::read_cajun()`
#' @param call the call for error messages
#' @returns A vpts for now.
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
  search_paths <- format_paths_local_vpts(
    rounded_interval = rounded_interval,
    path = path,
    radar = radar,
    format = getOption(
      "getRad.vpts_local_path_format_dark_ecology",
      default = "{year}/{month}/{day}/{radar}/"
    )
  )

  files <- purrr::map(
    search_paths,
    ~ .x |>
      purrr::keep(file.exists) |>
      purrr::map(.progress = "searching for local files", \(search_path) {
        # `{fs}` is much faster than base, and often already installed as it's a dependency of many tidyverse packages
        if (is_installed("fs")) {
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
      unlist() |>
      unname()
  )
  n_files <- purrr::map_int(files, length)
  if (all(n_files == 0)) {
    cli::cli_abort(
      c(
        x = "No files have been found for the radar{?s} specified ({.val {radar}}).",
        i = "The following paths have been searched {.file {unlist(search_paths)}}."
      ),
      call = call,
      class = "getRad_error_vpts_dark_ecology_no_files"
    )
  }
  if (any(n_files == 0)) {
    radars_no_data <- radar[n_files == 0]
    cli::cli_warn(
      c(
        x = "No files have been found for one or more the radars specified ({.val {radars_no_data}}).",
        i = "The following paths have been searched {.file {unlist(search_paths[n_files==0])}}.",
        i = "These files are considered missing data and therefore omitted from the results."
      ),
      call = call,
      class = "getRad_warning_vpts_dark_ecology_no_files_for_some_radars"
    )
    files <- files[n_files != 0]
  }
  purrr::map(files, \(y) {
    bioRad::bind_into_vpts(purrr::map(y, bioRad::read_cajun, ...))
  })
}
