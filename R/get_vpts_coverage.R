#' Get coverage for vpts from various sources
#'
#' @param source Source of the data. One or more of `"baltrad"`, `"uva"`,
#'   `"ecog-04003"` or `"rmi"`. If no source is provided `baltrad` is used.
#' @param ... Arguments passed on to internal functions.
#'
#' @returns A `data.frame` or `tibble` with at least three columns, `source`,
#'   `radar` and `date` to indicate the combination for which data exists
#'
#' @details
#' ```{r get url to fetch coverage from, echo = FALSE, results = FALSE}
#' cov_url <- paste(
#'   getOption("getRad.aloft_data_url"), "coverage.csv", sep = "/"
#' )
#' ```
#'
#' The coverage file for aloft is fetched from <`r cov_url`>. This can be
#' changed by setting `options(getRad.aloft_data_url)` to any desired url.
#'
#' @export
#'
#' @examplesIf interactive()
#' get_vpts_coverage()
get_vpts_coverage <- function(source = c("baltrad", "uva", "ecog-04003", "rmi"),
                              ...) {
  # Allow multiple sources, but only default values.
  source <- rlang::arg_match(source, multiple = TRUE)

  if (length(source) == 0) {
    cli::cli_abort("Source should atleast have one value.",
      class = "getRad_error_length_zero"
    )
  }

  # Create a mapping of sources to helper functions.
  # fn_map <- list(
  #   rmi = get_vpts_coverage_rmi,
  #   baltrad = get_vpts_coverage_aloft,
  #   uva = get_vpts_coverage_aloft,
  #   "ecog-04003" = get_vpts_coverage_aloft
  # )
  fn_map <-
    # List all coverage helpers, and get the default values of their source column.
    # If not present, infer the source value from the helper name
    # list the get_vpts helper names
    get_vpts_helper_names <-
    ls("package:getRad", pattern = "get_vpts") |>
    string_extract("get_vpts_(?!coverage).+")
  # Infer the coverage helper names
  get_vpts_coverage_names <-
    string_replace(get_vpts_helper_names, "(?<=get_vpts)_", "_coverage_")

  fn_map <- purrr::map(get_vpts_helper_names, ~ {
    source_value <- eval(formals(.x)$source)
    if (is.null(source_value)) {
      string_extract(.x, "(?<=get_vpts_).+")
    } else {
      source_value
    }
  }) |>
    purrr::set_names(get_vpts_coverage_names) |>
    # Create a list of source value : helper_function pairs
    purrr::imap(~ purrr::set_names(rep(.y, length(.x)), .x)) |>
    purrr::flatten() |>
    # Get the function instead of just its name
    purrr::map(get)

  # Run the helpers, but every helper only once.
  purrr::map(fn_map[source][!duplicated(fn_map[source])], \(helper_fn) helper_fn(...)) |>
    dplyr::bind_rows() |>
    dplyr::filter(source %in% !!source) |>
    dplyr::relocate("source", "radar", "date")
}
