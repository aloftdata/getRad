#' Extracts a substring from a string based on a regex pattern
#'
#' This function uses regular expressions to extract a substring from a given
#' string based on a specified pattern. This is a base replacement of
#' stringr::str_extract().
#'
#' @param string The input string from which the substring will be extracted.
#' @param pattern The regular expression pattern used to match the substring.
#' @return The extracted substring.
#' @noRd
#' @examples
#' string_extract("Hello World", "o W")
string_extract <- function(string, pattern) {
  regmatches(string, regexpr(pattern, text = string, perl = TRUE))
}

#' Replace a pattern in a string with a replacement
#'
#' This function uses regular expressions to replace a pattern in a string with
#' a specified replacement. This is a base replacement of
#' stringr::str_replace().
#'
#' @param string The input string.
#' @param pattern The pattern to search for in the string.
#' @param replacement The replacement string.
#' @return The modified string with the pattern replaced.
#' @noRd
#' @examples
#' string_replace("I'm looking for radars", "radar", "bird")
string_replace <- function(string, pattern, replacement) {
  sub(pattern, replacement, string, perl = TRUE)
}

#' Round a lubridate interval
#'
#' Extension of [lubridate::round_date()] to round an interval, by default by
#' day. This means that of any given interval, the function will return the
#' interval as a floor of the interval start, to the ceiling of the interval
#' end.
#'
#' @inheritParams lubridate::round_date
#' @return An interval starting with the floor of `x` and ending with the
#'   ceiling of `x`, by the chosen unit.
#' @noRd
#' @examples
#' round_interval(lubridate::interval("20230104 143204", "20240402 001206"))
round_interval <- function(x, unit = "day") {
  lubridate::interval(
    lubridate::floor_date(lubridate::int_start(x), unit),
    lubridate::ceiling_date(lubridate::int_end(x), unit)
  )
}

#' Get the end of the day for a given datetime
#'
#' @param date A datetime object or a character string that can be coerced to a
#'   datetime object.
#' @return A datetime object representing the end of the day.
#' @noRd
#' @examples
#' end_of_day("2016-03-05")
#' end_of_day("2020-07-12 11:01:33")
end_of_day <- function(date) {
  lubridate::floor_date(lubridate::as_datetime(date), "day") +
    lubridate::ddays(1) -
    lubridate::dseconds(1)
}

#' Set the list names to the unique value of the radar column
#'
#' @param vpts_df_list A list of vpts data frames.
#' @return A list of vpts data frames with the names set to the unique value of
#'   the radar column of the data frames.
#' @noRd
#' @examples
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
#' Convert a character vector to integer, but do not warn
#'
#' This function does not perform coercion, but conversion. For coercion see
#' vctrs::vec_cast().
#'
#' @param x A character vector.
#' @return An integer vector.
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
#' @return A logical vector.
#' @noRd
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

#' Convert a character vector to numeric, but do not warn
#'
#' This function does not perform coercion, but conversion. For coercion see
#' vctrs::vec_cast().
#'
#' @param x A character vector.
#' @return A numeric vector.
#' @noRd
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
#' @param req A `httr2` request.
#' @returns A `httr2` request.
#' @noRd
req_user_agent_getrad <- function(req) {
  httr2::req_user_agent(req, string = getOption("getRad.user_agent"))
}

#' Function to retry a getRad specific httr2 request
#'
#' This function retries the request if the response status is 429. It retries
#' the request 15 times with a backoff of 2 times the square root of the number
#' of tries It retries on failure.
#'
#' @param req A `httr2` request.
#' @param transient_statuses A vector of status codes that are considered
#'   transient and should be retried.
#' @param max_tries The maximum number of times to retry the request.
#' @returns A `httr2` request.
#' @noRd
req_retry_getrad <- function(req,
                             transient_statuses = c(429),
                             max_tries = 15,
                             retry_on_failure = TRUE) {
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
                               default = 6 * 60 * 60
                             ),
                             max_n = getOption("getRad.max_cache_n",
                               default = Inf
                             ),
                             max_size = getOption("getRad.max_cache_size_bytes",
                               default = 1024 * 1024 * 1024
                             ),
                             ...) {
  # If caching is disabled, return early.
  if (!use_cache) {
    return(req)
  }

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

#' Functions for checking odim codes
#'
#' @param x Character to be tested if they are odim codes.
#' @returns A logical the same length as `x` or an error if it does not match
#'   in the check functions.
#' @noRd
is_odim <- function(x) {
  if (length(x) < 1) {
    return(FALSE)
  }
  rlang::is_character(x) & !is.na(x) & grepl("^[a-zA-Z]{5}$", x)
}
is_nexrad <- function(x) {
  if (length(x) < 1) {
    return(FALSE)
  }
  rlang::is_character(x) & !is.na(x) & grepl("^[A-Za-z]{4}$", x)
}
is_odim_nexrad <- function(x) {
  is_odim(x) | is_nexrad(x)
}
is_odim_scalar <- function(x) {
  rlang::is_scalar_character(x) && all(is_odim(x))
}
is_odim_nexrad_scalar <- function(x) {
  rlang::is_scalar_character(x) && is_odim_nexrad(x)
}
check_odim <- function(x) {
  if (!all(is_odim(x))) {
    cli::cli_abort(
      "Please provide one or more radars as a character vector.
      Consisting of 5 characters each to match an odim code.",
      class = "getRad_error_radar_not_odim_string"
    )
  }
}
check_odim_nexrad <- function(x) {
  if (!all(is_odim_nexrad(x))) {
    cli::cli_abort(
      "Each element of {.arg radar} must be either a 5-letter ODIM code
      or a 4-letter NEXRAD ICAO code.",
      class = "getRad_error_radar_not_odim_nexrad"
    )
  }
  invisible(TRUE)
}
check_odim_scalar <- function(x) {
  if (!is_odim_scalar(x)) {
    cli::cli_abort(
      "Please provide radar as a character vector of length 1.
    Consisting of 5 characters to match an odim code.",
      class = "getRad_error_radar_not_single_odim_string"
    )
  }
}
check_odim_nexrad_scalar <- function(x) {
  if (!is_odim_nexrad_scalar(x)) {
    cli::cli_abort(
      "Radar must be exactly one 5-letter ODIM code or one 4-letter NEXRAD code.",
      class = "getRad_error_radar_not_single_odim_nexrad"
    )
  }
  invisible(TRUE)
}
#' Create an .onload function to set package options during load
#'
#' - getRad.key_prefix is the default prefix used when setting or getting
#' secrets using keyring.
#' - getRad.user_agent is the string used as a user agent for the http calls
#' generated in this package. It incorporates the package version using
#' `getNamespaceVersion`.
#' - getRad.max_cache_age_seconds is the default max cache age for the httr2
#' cache in seconds.
#' - getRad.max_cache_size_bytes is the default max cache size for the httr2
#' cache in bytes.
#' @noRd
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
