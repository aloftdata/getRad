#' A function to split out calls to internal functions for
#' reading data from local paths
#' @noRd
get_vpts_local <- function(
  radar,
  rounded_interval,
  source,
  path,
  ...,
  call = rlang::caller_env()
) {
  # Allow only one directory to be provided
  if (length(path) > 1) {
    cli::cli_abort(
      "Only one directory can be provided, but {length(path)} were given.",
      class = "getRad_error_vpts_local_multiple_directories",
      call = call
    )
  }

  if (!dir.exists(path)) {
    cli::cli_abort(
      c(
        x = "The provide path ({.file {path}}), is not a valid directory.",
        i = "Please make sure the directory exist."
      ),
      class = "getRad_error_path_not_a_dir",
      call = call
    )
  }
  # Check that the provided directory exists
  if (!is_readable(path)) {
    cli::cli_abort(
      "The provided directory does is not readable: {path}",
      class = "getRad_error_vpts_directory_not_readable",
      call = call
    )
  }

  fun <- (dplyr::case_when(
    source %in% c(c("baltrad", "uva", "ecog-04003")) ~ "get_vpts_local_aloft",
    source == "dark_ecology" ~ "get_vpts_local_dark_ecology",
    .default = NA
  ))
  if (rlang::is_na(fun)) {
    cli::cli_abort(
      "No suitable function exist to read local data for this source ({.val {source}}).",
      class = "getRad_error_no_function_for_reading_local_source",
      call = call
    )
  }
  rlang::exec(
    fun,
    radar = radar,
    rounded_interval = rounded_interval,
    path = path,
    ...
  )
}
