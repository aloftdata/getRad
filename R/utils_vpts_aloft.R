#' Reading VPTS files from a vector of URLs
#'
#' This is a simple helper to read a vector of URLs as VPTS files using vroom
#' and httr2. This wrapper makes use of purrr to create requests that can be
#' handled in parallel with our own custom retry settings and user agent.
#'
#' @details
#' Apart from parallelisation and these custom settings, this could also be
#' handled by simple call to `vroom::vroom(file = urls)`.
#'
#' This function also includes column specifications for the VPTS CSV data
#' standard. However, [bioRad::as.vpts()] currently doesn't support factors,
#' thus any fields sent to that function need to be parsed as character vectors.
#'
#' @param urls Character vector of URLs to VPTS files.
#' @inheritParams req_cache_getrad
#' @return A list of tibbles, one for each URL.
#' @noRd
#' @examples
#' c(
#'   "https://aloftdata.s3-eu-west-1.amazonaws.com/baltrad/daily/bejab/2024/bejab_vpts_20240305.csv",
#'   "https://aloftdata.s3-eu-west-1.amazonaws.com/baltrad/daily/bejab/2024/bejab_vpts_20240306.csv",
#'   "https://aloftdata.s3-eu-west-1.amazonaws.com/baltrad/daily/bejab/2024/bejab_vpts_20240307.csv"
#' ) |>
#'   read_vpts_from_url()
read_vpts_from_url <- function(urls, use_cache = TRUE) {
  ## this could also be done by passing the vector of urls to readr::read_csv()
  ## or vroom::vroom(), but both would be slower because they are not parallel
  ## and wouldn't declare our custom user agent or allow us to set retry
  ## criteria

  fetch_from_url_raw(urls, use_cache = use_cache) |>
    purrr::map(
      ~ if (length(.x)) {
        vroom::vroom(
          delim = ",",
          I(.x),
          col_types = getOption(
            "getRad.vpts_col_types"
          ),
          show_col_types = NULL,
          progress = FALSE
        )
      } else {
        (data.frame())
      }
    )
}
