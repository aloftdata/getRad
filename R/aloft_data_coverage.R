#' Get coverage of Aloft bucket
#'
#' Retrieves an overview of the number of hdf5 files per directory in the
#' [Aloft bucket](https://aloftdata.eu/browse/), based on the `coverage.csv`
#' file in that bucket. By default this file is cached for 6 hours.
#'
#' @param call A call used for error messaging
#'
#' @details
#' ```{r get url to fetch coverage from, echo = FALSE, results = FALSE}
#' cov_url <- paste(
#'   getOption("getRad.aloft_data_url"), "coverage.csv", sep = "/"
#' )
#' ```
#'
#' The coverage file is fetched from <`r cov_url`>. This can be changed by
#' setting `options(getRad.aloft_data_url)` to any desired url.
#'
#' @inheritParams req_cache_getrad
#' @return A data frame of the coverage file on the Aloft bucket.
#' @examplesIf interactive()
#' get_vpts_coverage_aloft()
get_vpts_coverage_aloft <- function(use_cache = TRUE, ..., call = rlang::caller_env()) {
  # Discover what data is available for the requested radar and time interval
  aloft_data_url <- getOption("getRad.aloft_data_url")
  coverage_raw <-
    httr2::request(aloft_data_url) |>
    httr2::req_url_path_append("coverage.csv") |>
    req_user_agent_getrad() |>
    req_retry_getrad() |>
    req_cache_getrad(use_cache = use_cache) |>
    httr2::req_progress(type = "down") |>
    httr2::req_perform(error_call = call) |>
    httr2::resp_body_raw()

  coverage <-
    vroom::vroom(coverage_raw, progress = FALSE, show_col_types = FALSE) |>
    dplyr::mutate(
      source = string_extract(.data$directory, ".+(?=\\/hdf5)"),
      radar = string_extract(.data$directory, "(?<=hdf5\\/)[a-z]{5}"),
      date = as.Date(
        string_extract(
          .data$directory,
          "[0-9]{4}\\/[0-9]{2}\\/[0-9]{2}$"
        )
      )
    )

  return(coverage)
}
