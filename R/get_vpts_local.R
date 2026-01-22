get_vpts_local <- function(
  radar,
  rounded_interval,
  directory,
  ...,
  call = rlang::caller_env()
) {
  dates <- as.Date(seq(
    lubridate::int_start(rounded_interval),
    lubridate::int_end(rounded_interval),
    "day"
  ))
  file_paths_list <- radar |>
    purrr::map(
      ~ unique(glue::glue(
        getOption(
          "getRad.vpts_local_path_format",
          default = "{radar}/{year}/{radar}_vpts_{year}{month}.csv.gz"
        ),
        radar = .x,
        year = lubridate::year(dates),
        month = sprintf("%02i", lubridate::month(dates)),
        day = sprintf("%02i", lubridate::day(dates)),
        date = dates
      ))
    ) |>
    purrr::set_names(radar)
  # `full_paths_list` is a list of file paths per radar, so that one vpts per radar is calculated
  full_paths_list <- purrr::map(file_paths_list, ~ file.path(directory, .x))
  full_paths_exist_list <- purrr::map(full_paths_list, file.exists)
  if (all(!unlist(full_paths_exist_list))) {
    cli::cli_abort(
      c(
        x = "None of the expected files are in the source directory ({.file {directory}}).",
        i = "The following files were expected: {.file {unlist(full_paths)}}."
      ),
      class = "getRad_error_files_not_in_source_dir",
      call = call
    )
  }
  if (any(!unlist(full_paths_exist_list))) {
    missing_files <- unlist(purrr::map2(
      full_paths_list,
      full_paths_exist_list,
      ~ .x[!.y]
    ))
    cli::cli_warn(
      c(
        x = "Some of the expected files are in the source directory ({.file {directory}}).",
        i = "The following files were expected but not found: {.file {missing_files}}.",
        i = "These files are considered missing data and therefore omitted from the results."
      ),
      missing_files = missing_files,
      class = "getRad_warning_some_files_not_in_source_dir",
      call = call
    )
  }
  any_file <- purrr::map_lgl(full_paths_exist_list, any)
  purrr::map2(
    full_paths_list[any_file],
    full_paths_exist_list[any_file],
    ~ vroom::vroom(
      .x[.y],
      col_types = getOption(
        "getRad.vpts_col_types"
      ),
      show_col_types = NULL,
      progress = FALSE
    ) |>
      tibble::add_column(source = directory) |>
      dplyr::mutate(dplyr::across("radar", as.character))
  )
}
