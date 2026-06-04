#' Get VPTS file coverage from the public BirdCast NEXRAD archive
#'
#' Gets the VPTS file coverage from the public BirdCast NEXRAD archive. This is
#' derived from a coverage file at
#' <`r file.path(getOption("getRad.birdcast_vpts_data_url"), "coverage.csv")`>. By
#' default this file is cached for 6 hours.
#'
#' @param ... Used to prevent accidentally using the `call` argument
#' @param call A call used for error messaging.
#' @inheritParams req_cache_getrad
#' @return A data frame of the coverage file in the birdcast VPTS archive.
#' @noRd
#' @examplesIf interactive()
#' get_vpts_coverage_birdcast()
get_vpts_coverage_birdcast <- function(
  use_cache = TRUE,
  ...,
  call = rlang::caller_env()
) {
  birdcast_vpts_data_url <- getOption("getRad.birdcast_vpts_data_url")

  coverage_raw <-
    httr2::request(birdcast_vpts_data_url) |>
    httr2::req_url_path_append("coverage.csv") |>
    req_user_agent_getrad() |>
    req_retry_getrad() |>
    req_cache_getrad(use_cache = use_cache) |>
    httr2::req_perform(error_call = call) |>
    httr2::resp_body_raw()

  coverage <-
    vroom::vroom(
      coverage_raw,
      progress = FALSE,
      show_col_types = FALSE
    ) |>
    dplyr::mutate(
      source = "birdcast",
      radar = string_extract(.data$directory, "(?<=daily\\/)[A-Z0-9]{4}"),
      date = as.Date(
        string_extract(
          .data$directory,
          "[0-9]{4}\\/[0-9]{2}\\/[0-9]{2}$"
        )
      )
    )

  return(coverage)
}
