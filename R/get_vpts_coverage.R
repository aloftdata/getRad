#' Get VPTS file coverage from supported sources
#'
#' Gets the VPTS file coverage from supported sources per radar and date.
#'
#' @param source Source of the data. One or more of `"baltrad"`, `"uva"`,
#'   `"ecog-04003"` or `"rmi"`. If not provided, `"baltrad"` is used.
#'   Alternatively `"all"` can be used if data from all sources should be
#'   returned.
#' @param ... Arguments passed on to internal functions.
#' @returns A `data.frame` or `tibble` with at least three columns, `source`,
#'   `radar` and `date` to indicate the combination for which data exists.
#' @export
#' @examplesIf interactive()
#' get_vpts_coverage()
get_vpts_coverage <- function(
  source = c("baltrad", "uva", "ecog-04003", "rmi"),
  ...
) {
  # argument all returns all possible sources
  if (rlang::is_scalar_character(source) && source == "all") {
    source <- rlang::eval_bare(formals(rlang::caller_fn(0))[["source"]])
  }
  if (missing(source)) {
    # If no source is provided, use baltrad.
    source <- "baltrad"
  } else {
    # Allow multiple sources, but only default values.
    source <- rlang::arg_match(source, multiple = TRUE)
  }

  if (length(source) == 0) {
    cli::cli_abort(
      "Source should atleast have one value.",
      class = "getRad_error_length_zero"
    )
  }

  # Create a mapping of sources to helper functions.
  fn_map <- list(
    rmi = get_vpts_coverage_rmi,
    baltrad = get_vpts_coverage_aloft,
    uva = get_vpts_coverage_aloft,
    "ecog-04003" = get_vpts_coverage_aloft
  )
  cl <- rlang::caller_env(0)
  # Run the helpers, but every helper only once.
  purrr::map(
    fn_map[source][!duplicated(fn_map[source])],
    \(helper_fn) helper_fn(...),
    .purrr_error_call = cl
  ) |>
    dplyr::bind_rows() |>
    dplyr::filter(source %in% !!source) |>
    dplyr::relocate("source", "radar", "date")
}
