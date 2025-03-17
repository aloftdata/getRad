#' Fetch the coverage table from the aloft data repository
#'
#' The coverage table provides an overview of what files are available on the
#' aloft data repository. Specifically it lists the directories that are
#' available as well as the number of files in every directory. By default this
#' file is cached for 6 hours.
#'
#' @param use_cache Logical indicating whether to use the cache. Default is
#'  `TRUE`. If `FALSE` the cache is ignored and the file is fetched from the
#'  aloft data repository. This can be useful if you want to force a refresh of
#'  the cache.
#'
#' @return A data.frame of the coverage file on the aloft data repository
#' @export
#'
#' @examplesIf interactive()
#' aloft_data_coverage()
aloft_data_coverage <- function(use_cache = TRUE) {
  # Discover what data is available for the requested radar and time interval
  coverage_url <- "https://aloftdata.s3-eu-west-1.amazonaws.com/coverage.csv"
  coverage_raw <-
    httr2::request(coverage_url) |>
    req_user_agent_getrad() |>
    httr2::req_retry() |>
    (\(request) if (use_cache) {
      httr2::req_cache(
        request,
        path =
          file.path(
            tools::R_user_dir("getRad", "cache"),
            "httr2"
          ),
        max_age = 6 * 60 * 60
        )
    } else {
      request
    })() |>
    httr2::req_progress(type = "down") |>
    httr2::req_perform() |>
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
