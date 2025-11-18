get_vpts_local <- function(
  radar,
  rounded_interval,
  directory
) {
  dates <- as.Date(seq(
    lubridate::int_start(rounded_interval),
    lubridate::int_end(rounded_interval),
    "day"
  ))
  file_paths <- radar |>
    purrr::map(
      ~ unique(glue::glue(
        getOption(
          "getRad.vpts_local_path_format",
          default = "{radar}/{year}/{radar}_vpts_{year}{sprintf('%02i',month)}.csv.gz"
        ),
        radar = .x,
        year = lubridate::year(dates),
        month = lubridate::month(dates),
        day = lubridate::day(dates),
        datetime = strftime(dates, "%Y%m%d%H%M", tz = "UTC")
      ))
    ) |>
    purrr::set_names(radar)
  full_paths <- purrr::map(file_paths, ~ file.path(directory, .x))
  s <- purrr::map(full_paths, file.exists)
  if (all(!unlist(s))) {
    cli::cli_abort(
      c(
        x = "None of the expected files are in the source directory ({.file {directory}}).",
        i = "The following files were expected: {.file {unlist(full_paths)}}."
      ),
      class = "getRad_error_files_not_in_source_dir"
    )
  }
  if (any(!unlist(s))) {
    missing_files <- unlist(purrr::map2(full_paths, s, ~ .x[!.y]))
    cli::cli_warn(
      c(
        x = "Some of the expected files are in the source directory ({.file {directory}}).",
        i = "The following files were expected but not found: {.file {missing_files}}.",
        i = "These files are considered missing data and therefore omitted from the results."
      ),
      missing_files = missing_files,
      class = "getRad_warning_some_files_not_in_source_dir"
    )
  }
  any_file <- purrr::map_lgl(s, any)
  purrr::map2(
    full_paths[any_file],
    s[any_file],
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
