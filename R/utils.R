#' Extracts a substring from a string based on a regex pattern.
#'
#' This function uses regular expressions to extract a substring from a given string
#' based on a specified pattern. This is a base replacement of stringr::str_extract()
#'
#' @param string The input string from which the substring will be extracted.
#' @param pattern The regular expression pattern used to match the substring.
#'
#' @return The extracted substring.
#' @noRd
#'
#' @examples
#' string_extract("Hello World", "o W")
#'
string_extract <- function(string, pattern) {
  regmatches(string, regexpr(pattern, text = string, perl = TRUE))
}

#' Replace a pattern in a string with a replacement.
#'
#' This function uses regular expressions to replace a pattern in a string with a specified replacement.
#' This is a base replacement of stringr::str_replace()
#'
#' @param string The input string.
#' @param pattern The pattern to search for in the string.
#' @param replacement The replacement string.
#'
#' @return The modified string with the pattern replaced.
#' @noRd
#' @examples
#' string_replace("I'm looking for radars", "radar", "bird")
#'
string_replace <- function(string, pattern, replacement) {
  sub(pattern, replacement, string, perl = TRUE)
}


#' Replace all occurrences of a pattern in a string with a replacement.
#'
#' This function uses regular expressions to replace all occurrences of a
#' pattern in a string with a specified replacement. This is a base replacement
#' of stringr::str_replace_all()
#'
#' @param string The input string.
#' @param pattern The pattern to search for in the string.
#' @param replacement The replacement string.
#'
#' @return The modified string with all occurrences of the pattern replaced.
#' @noRd
#'
#' @examples
#' string_replace_all("starwars", "wars", "trek")
string_replace_all <- function(string, pattern, replacement) {
  gsub(pattern, replacement, string, perl = TRUE)
}

#' Remove all whitespace from a string from both ends.
#'
#' This function uses regular expressions to remove all whitespace from a
#' string. This is a base replacement of stringr::str_squish()
#'
#' @param string The input string.
#'
#' @return A string with all whitespace removed from both ends.
#' @noRd
#'
#' @examples
#' string_squish("  aoosh  ")
#' string_squish(" A sentence with extra whitespace.   ")
string_squish <- function(string){
  string_replace_all(string, "^\\s+", "") |>
    string_replace_all("\\s+$", "")
}

#' Round a lubridate interval
#'
#' Extension of [lubridate::round_date()] to round an interval, by default by
#' day. This means that of any given interval, the function will return the
#' interval as a floor of the interval start, to the ceiling of the interal end.
#'
#' @inheritParams lubridate::round_date
#'
#' @return An interval starting with the floor of `x` and ending with the
#'   ceiling of `x`, by the chosen unit.
#' @noRd
#'
#' @examples
#' round_interval(lubridate::interval("20230104 143204", "20240402 001206"))
round_interval <- function(x, unit = "day"){
  lubridate::interval(
    lubridate::floor_date(lubridate::int_start(x), unit),
    lubridate::ceiling_date(lubridate::int_end(x), unit)
  )
}

#' Get the end of the day for a given datetime
#'
#' @param date A datetime object or a character string that can be coerced to a
#'   datetime object.
#'
#' @return A datetime object representing the end of the day.
#' @noRd
#'
#' @examples
#' end_of_day("2016-03-05")
#' end_of_day("2020-07-12 11:01:33")
end_of_day <- function(date){
  lubridate::floor_date(lubridate::as_datetime(date), "day") +
    lubridate::ddays(1) -
    lubridate::dseconds(1)
}

#' Set the list names to the unique value of the radar column
#'
#'
#' @param vpts_df_list A list of vpts data.frames
#'
#' @return A list of vpts data.frames with the names set to the unique value of
#'   the radar column of the data.frames
#'
#' @noRd
#' @examples
#'
#' list(dplyr::tibble(radar = "bejab"), dplyr::tibble(radar = "bewid")) |>
#'   radar_to_name()
radar_to_name <- function(vpts_df_list) {
  purrr::set_names(
    vpts_df_list,
    purrr::map_chr(
      vpts_df_list,
      \(df) unique(dplyr::pull(df, .data$radar))
    )
  )
}
#' Convert a character vector to integer, but do not warn.
#'
#' This function does not perform coercion, but conversion. For coercion see
#' [vctrs::vec_cast()](https://vctrs.r-lib.org/reference/vec_cast.html).
#'
#'
#' @param x A character vector
#'
#' @return An integer vector
#' @seealso [as_numeric_shh()] [as_logical_shh()]
#'
#' @noRd
#'
#' @examples
#' as_integer_shh(c("1", "2", "3"))
as_integer_shh <- function(x) {
  if (!is.character(x)) {
    cli::cli_abort("x must be a character vector")
  }
  suppressWarnings(as.integer(x))
}

#' Convert a character vector containing `Ỳ`, `N` and `NA` to a logical vector.
#'
#' @param x A character vector only containing `Y`, `N` and `NA`. Any other
#'   values will be silenty converted to `NA`.
#'
#' @return A logical vector
#' @seealso [as_numeric_shh()] [as_integer_shh()]
#'
#' @noRd
#'
#' @examples
#' yes_no_as_logical(c("Y", "N", NA, NA, "Y"))
#' yes_no_as_logical(c("Y", "foo", "bar", "N", NA))
yes_no_as_logical <- function(x) {
  # x needs to be a character vector
  if (!is.character(x)) {
    cli::cli_abort("x must be a character vector")
  }

  # Convert `Y` to TRUE, `N` to FALSE and `NA` to NA
  converted_vector <-
    dplyr::case_when(
      x == "Y" ~ TRUE,
      x == "N" ~ FALSE,
      .default = NA,
      .ptype = logical()
    )

  return(converted_vector)
}

#' Convert a character vector to numeric, but do not warn.
#'
#' This function does not perform coercion, but conversion. For coercion see
#' [vctrs::vec_cast()](https://vctrs.r-lib.org/reference/vec_cast.html).
#'
#' @param x A character vector
#'
#' @return A numeric vector
#' @seealso [as_integer_shh()] [as_logical_shh()]
#'
#' @noRd
#'
#' @examples
#' as_double_shh(c("1.1", "2.2", "3.3"))
as_numeric_shh <- function(x) {
  if (!is.character(x)) {
    cli::cli_abort("x must be a character vector")
  }
  suppressWarnings(as.numeric(x))
}

#' Function to set the user agent to a getRad specific one in an httr2 request
#'
#' @param req an `httr2` request
#'
#' @returns an `httr2` request
#' @noRd
req_user_agent_getrad <- function(req) {
  httr2::req_user_agent(req, string = getOption("getRad.user_agent"))
}

#' Function to retry a getRad specific httr2 request This function retries the
#' request if the response status is 429 It retries the request 15 times with a
#' backoff of 2 times the square root of the number of tries It retries on
#' failure
#'
#' @param req an `httr2` request
#' @param transient_statuses a vector of status codes that are considered
#'   transient and should be retried
#' @param max_tries the maximum number of times to retry the request
#'
#' @returns an `httr2` request
#' @noRd
req_retry_getrad <- function(req,
                             transient_statuses = c(429),
                             max_tries = 15,
                             retry_on_failure = TRUE){
  httr2::req_retry(
    req,
    max_tries = max_tries,
    backoff = \(x) sqrt(x) * 2,
    is_transient = \(resp) httr2::resp_status(resp) %in% transient_statuses,
    retry_on_failure = retry_on_failure
  )
}

#' Function to set the cache for a getRad specific httr2 request
#'
#' @inheritParams httr2::req_cache
#' @param req A `httr2` request.
#' @param use_cache Logical indicating whether to use the cache. Default is
#'   `TRUE`. If `FALSE` the cache is ignored and the file is fetched anew.
#'    This can also be useful if you want to force a refresh of the cache.
#' @param ... Additional arguments passed to `httr2::req_cache()`.
#' @keywords internal
req_cache_getrad <- function(req,
                             use_cache = TRUE,
                             max_age = getOption("getRad.max_cache_age_seconds",
                                                 default = 6 * 60 * 60),
                             max_n = getOption("getRad.max_cache_n",
                                               default = Inf),
                             max_size = getOption("getRad.max_cache_size_bytes",
                                                  default = 1024 * 1024 * 1024),
                             ...){
  # If caching is disabled, return early.
  if(!use_cache){return(req)}

  httr2::req_cache(
    req,
    path =
      file.path(
        tools::R_user_dir("getRad", "cache"),
        "httr2"
      ),
    max_age = max_age,
    max_n = max_n,
    max_size = max_size,
    ...
  )
}

#' Functions for checking odim codes.
#'
#' @param x A character to be tested if they are odim codes
#'
#' @returns An logical the same length as `x` or an error if it does not match
#'   in the check functions
#' @noRd
is_odim<-function(x){
  if(length(x)<1){
    return(FALSE)
    }
  rlang::is_character(x) & !is.na(x) & grepl("^[a-zA-Z]{5}$",x)
}
is_odim_scalar<-function(x){
  rlang::is_scalar_character(x) && all(is_odim(x))
}
check_odim<-function(x){
  if(!all(is_odim(x)))
  {
    cli::cli_abort(
      "Please provide one or more radars as a character vector.
      Consisting of 5 characters each to match an odim code.",
      class = "getRad_error_radar_not_odim_string"
    )
  }
}
check_odim_scalar<-function(x){
  if(!is_odim_scalar(x))
    cli::cli_abort(
    "Please provide radar as a character vector of length 1.
    Consisting of 5 characters to match an odim code.",
    class = "getRad_error_radar_not_single_odim_string"
  )
}


#' Replace "nan" with NaN in a string
#'
#' @param string A character vector that may contain "nan" values.
#'
#' @return A numeric vector where "nan" values are replaced with NaN and other
#' @noRd
#' @examples
#' replace_nan_numeric(c("44", "-95.6", "nan", 88))
replace_nan_numeric <- function(string) {
  as.numeric(replace(string, string == "nan", NaN))
}

#' Fetch data from a list of URLs and return the raw response bodies.
#'
#' @param url A character vector of URLs to fetch data from.
#' @param use_cache A logical value indicating whether to use caching for the
#'  requests. Default is TRUE.
#'
#' @return A list of raw response bodies from the URLs.
#' @noRd
fetch_from_url_raw <- function(urls, use_cache = TRUE){
  purrr::map(urls, httr2::request) |>
    # Identify ourselves in the request
    purrr::map(req_user_agent_getrad) |>
    # Set retry conditions
    purrr::map(req_retry_getrad) |>
    # Optionally cache the responses
    (\(request_list) if (use_cache) {
      purrr::map(
        request_list,
        \(request) {
          httr2::req_cache(request,
                           path = file.path(
                             tools::R_user_dir("getRad", "cache"),
                             "httr2"
                           ),
                           max_age = getOption("getRad.max_cache_age_seconds"),
                           max_size = getOption("getRad.max_cache_size_bytes")
          )
        }
      )
    } else {
      request_list
    })() |>
    # Perform the requests in parallel
    httr2::req_perform_parallel() |>
    # Fetch the response bodies and parse it using vroom
    ## A helper in bioRad (validate_vpts()) that we call indirectly via
    # " bioRad::as.vpts() currently doesn't support factors: bioRad v0.8.1
    purrr::map(httr2::resp_body_raw)
}

#' Read lines from a list of URLs and return them as a list of character
#' vectors.
#'
#' @param urls A character vector of URLs to read lines from.
#' @param use_cache A logical value indicating whether to use caching for the
#'   requests.
#'
#' @return A list of character vectors, each containing the lines read from the
#'  corresponding URL.
#' @noRd
#'
#' @examples
#' read_lines_from_url(
#'     file.path("https://raw.githubusercontent.com/philspil66",
#'               "Super-Star-Trek/refs/heads/main/superstartrek.bas"))
read_lines_from_url <- function(urls, use_cache = TRUE) {
  fetch_from_url_raw(urls, use_cache = use_cache) |>
    I() |>
    purrr::map(~ vroom::vroom_lines(.x,
                                    progress = FALSE
    ))
}

# Create an .onload function to set package options during load
# getRad.key_prefix is the default prefix used when setting or getting secrets using keyring
# getRad.user_agent is the string used as a user agent for the http calls generated in this package
#   It incorporates the package version using `getNamespaceVersion`
# getRad.max_cache_age_seconds is the default max cache age for the httr2 cache
# in seconds
# getRad.max_cache_size_bytes is the default max cache size for the httr2 cache
# in bytes
.onLoad <- function(libname, pkgname) { # nolint

  op <- options()
  op.getRad <- list(
    getRad.key_prefix = "getRad_",
    getRad.user_agent = paste("R package getRad", getNamespaceVersion("getRad")),
    getRad.aloft_data_url = "https://aloftdata.s3-eu-west-1.amazonaws.com"
  )
  toset <- !(names(op.getRad) %in% names(op))
  if (any(toset)) options(op.getRad[toset])
  rlang::run_on_load()
  invisible()
}
rlang::on_load(rlang::local_use_cli(inline = TRUE))
