#' Match radars to a specific arguments/name
#'
#' @param radar The radar to match.
#' @param ... A set of radar names with their corresponding mapping.
#' @param call The caller environment for the error messages.
#'
#' @returns (character) The resulting radar mapping.
#' @noRd
#'
#' @examples
#' radar_recode("nlhrw", "nldhl" = "Den Helder", "nlhrw" = "Herwijnen")
radar_recode <- function(radar, ..., call = rlang::caller_env()) {
  if (!(rlang::is_scalar_character(radar) && !is.na(radar))) {
    cli::cli_abort(
      "The argument {.arg radar} should be scalar character of length 1 that is not NA.",
      class = "getRad_error_recode_radar_radar_argument",
      call = call
    )
  }
  res <- switch(
    radar,
    ...,
    cli::cli_abort(
      c(
        x = "No mapping exists for the {.val {radar}} radar.",
        i = " Either this radar is non-existant (possibly check with {.code get_weather_radars()}). Alternatively no mapping is (yet) implemented for this radar. In the later case consider creating a bug report."
      ),
      class = "getRad_error_radar_not_found",
      call = call
    )
  )
  return(res)
}


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
  regmatches(string, regexpr(pattern, text = string, perl = TRUE)) |>
    as.character() # avoid returning class glue
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

#' Replace all occurrences of a pattern in a string with a replacement
#'
#' This function uses regular expressions to replace all occurrences of a
#' pattern in a string with a specified replacement. This is a base replacement
#' of stringr::str_replace_all().
#'
#' @param string The input string.
#' @param pattern The pattern to search for in the string.
#' @param replacement The replacement string.
#' @return The modified string with all occurrences of the pattern replaced.
#' @noRd
#' @examples
#' string_replace_all("starwars", "wars", "trek")
string_replace_all <- function(string, pattern, replacement) {
  gsub(pattern, replacement, string, perl = TRUE)
}

#' Remove all whitespace from a string from both ends
#'
#' This function uses regular expressions to remove all whitespace from a
#' string. This is a base replacement of stringr::str_squish().
#'
#' @param string The input string.
#' @return A string with all whitespace removed from both ends.
#' @noRd
#' @examples
#' string_squish("  aoosh  ")
#' string_squish(" A sentence with extra whitespace.   ")
string_squish <- function(string) {
  string_replace_all(string, "^\\s+", "") |>
    string_replace_all("\\s+$", "")
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
#' @param date Datetime object or a character string that can be coerced to a
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

#' Calculate the mean of the radar cross section
#'
#' bioRad::as.vpts() only uses the first value for rcs provided, so while we can
#' calculate it by eta/dens, it makes sense to only use the most common value
#' when transforming to vpts objects.
#'
#' This function drops `NA`, `NaN` and `Inf` for calculating the mean.
#'
#' @param eta Animal reflectivity,
#' @param dens Animal density.
#' @return A numeric value representing the mean radar cross section (rcs).
#' @noRd
calc_single_mean_rcs <- function(eta, dens) {
  rcs <- eta / dens
  # Omit NA, NaN and Inf
  rcs[is.nan(rcs) | is.infinite(rcs)] <- NA
  # Get the mean as bioRad::as.vpts() only uses the first value anyway.
  mean(rcs, na.rm = TRUE)
}

#' Set the list names to the unique value of the radar column
#'
#' @param vpts_df_list List of vpts data frames.
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
#' @param x Character vector.
#' @return An integer vector.
#' @seealso [as_numeric_shh()] [as_logical_shh()]
#' @noRd
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
#' @param x Character vector only containing `Y`, `N` and `NA`. Any other
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
#' @param x Character vector.
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
#' @param req `httr2` request.
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
req_retry_getrad <- function(
  req,
  transient_statuses = c(429),
  max_tries = 15,
  retry_on_failure = TRUE
) {
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
#' @param req `httr2` request.
#' @param use_cache Logical indicating whether to use the cache. Default is
#'   `TRUE`. If `FALSE` the cache is ignored and the file is fetched anew.
#'    This can also be useful if you want to force a refresh of the cache.
#' @param ... Additional arguments passed to `httr2::req_cache()`.
#' @keywords internal
req_cache_getrad <- function(
  req,
  use_cache = TRUE,
  max_age = getOption("getRad.max_cache_age_seconds", default = 6 * 60 * 60),
  max_n = getOption("getRad.max_cache_n", default = Inf),
  max_size = getOption(
    "getRad.max_cache_size_bytes",
    default = 1024 * 1024 * 1024
  ),
  ...
) {
  # If caching is disabled, return early.
  if (!use_cache) {
    return(req)
  }

  httr2::req_cache(
    req,
    path = file.path(
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
check_odim <- function(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!all(is_odim(x))) {
    cli::cli_abort(
      "Please provide one or more radars as a character vector.
      Consisting of 5 characters each to match an odim code.",
      class = "getRad_error_radar_not_odim_string",
      call = call
    )
  }
}
check_odim_nexrad <- function(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!all(is_odim_nexrad(x))) {
    cli::cli_abort(
      "Each element of {.arg {arg}} must be either a 5-letter ODIM code
      or a 4-letter NEXRAD ICAO code.",
      class = "getRad_error_radar_not_odim_nexrad",
      call = call
    )
  }
  invisible(TRUE)
}
check_odim_scalar <- function(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is_odim_scalar(x)) {
    cli::cli_abort(
      "Please provide {.arg {arg}} as a character vector of length 1.
    Consisting of 5 characters to match an odim code.",
      class = "getRad_error_radar_not_single_odim_string",
      call = call
    )
  }
}

check_odim_nexrad_scalar <- function(
  x,
  ...,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is_odim_nexrad_scalar(x)) {
    cli::cli_abort(
      "Radar must be exactly one 5-letter ODIM code or one 4-letter NEXRAD code.",
      class = "getRad_error_radar_not_single_odim_nexrad",
      call = call
    )
  }
  invisible(TRUE)
}

#' Replace "nan" with NaN in a string
#'
#' @param string Character vector that may contain `"nan"` values.
#' @return A numeric vector where `"nan"` values are replaced with NaN.
#' @noRd
#' @examples
#' replace_nan_numeric(c("44", "-95.6", "nan", 88))
replace_nan_numeric <- function(string) {
  as.numeric(replace(string, string == "nan", NaN))
}

#' Fetch data from a list of URLs and return the raw response bodies
#'
#' @param url Character vector of URLs to fetch data from.
#' @param use_cache Logical value indicating whether to use caching for the
#'  requests. Default is `TRUE`.
#' @return A list of raw response bodies from the URLs.
#' @noRd
fetch_from_url_raw <- function(urls, use_cache = TRUE, parallel = TRUE) {
  data_request <- purrr::map(urls, httr2::request) |>
    # Identify ourselves in the request
    purrr::map(req_user_agent_getrad) |>
    # Set retry conditions
    purrr::map(req_retry_getrad) |>
    # Set throttling so we don't overwhelm data sources
    purrr::map(\(req) {
      httr2::req_throttle(req, capacity = 30, fill_time_s = 40)
    }) |>
    # Optionally cache the responses
    purrr::map(req_cache_getrad)
  # Perform the requests in parallel or sequentially
  if (parallel) {
    data_response <-
      data_request |>
      httr2::req_perform_parallel(
        progress = interactive(),
        on_error = "continue"
      )
  } else {
    data_response <-
      data_request |>
      httr2::req_perform_sequential(on_error = "continue")
  }
  # Make warning for missing csv
  if (any(ss <- unlist(lapply(data_response, inherits, "httr2_http_404")))) {
    cli::cli_warn(
      class = "getRad_warning_404_on_csv_download",
      c(
        "!" = "The following: {urls[ss]} url{?s} could not be downloaded (HTTP 404 Not Found).",
        i = "Given an attempt was made data was present in the coverage data. Therefore this likely relates to an error in the data repository. For now the data has been omitted from the returned result however for a final resolution the issue should be resolved in the repository (e.g. {.url https://github.com/aloftdata/data-repository})."
      )
    )
    for (i in seq_along(data_response)) {
      if (ss[i]) {
        data_response[[i]] <- raw()
      } else {
        data_response[[i]] <- httr2::resp_body_raw(data_response[[i]])
      }
    }
    return(data_response)
  }
  # Fetch the response bodies
  purrr::map(data_response, httr2::resp_body_raw)
}

#' Read lines from a list of URLs and return them as a list of character
#' vectors
#'
#' @param urls Character vector of URLs to read lines from.
#' @param use_cache Logical value indicating whether to use caching for the
#'   requests.
#' @return A list of character vectors, each containing the lines read from the
#'  corresponding URL.
#' @noRd
#' @examples
#' read_lines_from_url(
#'   file.path(
#'     "https://raw.githubusercontent.com/philspil66",
#'     "Super-Star-Trek/refs/heads/main/superstartrek.bas"
#'   )
#' )
read_lines_from_url <- function(urls, use_cache = TRUE, parallel = TRUE) {
  fetch_from_url_raw(urls, use_cache = use_cache, parallel) |>
    I() |>
    purrr::map(~ vroom::vroom_lines(.x, progress = FALSE))
}

#' Get HTML from a URL
#'
#' @param url URL to get the HTML from.
#' @param use_cache Logical. If `TRUE`, use the cache. If `FALSE`, do not use
#'   the cache.
#' @return HTML content from the URL as a xml2 html object.
#' @noRd
get_html <- function(url, use_cache = TRUE, ..., call = rlang::caller_env()) {
  httr2::request(url) |>
    req_user_agent_getrad() |>
    req_retry_getrad() |>
    req_cache_getrad(use_cache = use_cache) |>
    httr2::req_perform(error_call = call) |>
    httr2::resp_body_html()
}

#' Get an html element using regex selection from a html object.
#'
#' @param html html object from the `xml2` package.
#' @param regex regex to select the element.
#' @return A character vector with the selected elements.
#' @noRd
get_element_regex <- function(html, regex) {
  html |>
    xml2::xml_find_all(".//a") |>
    xml2::xml_text() |>
    string_extract(regex) |>
    (\(vec) vec[!is.na(vec)])()
}
