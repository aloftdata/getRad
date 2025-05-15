# Helpers to parse/coerce values from RMI FWF VPTS ------------------------

#' Parse numeric values from RMI VPTS data
#'
#' This function is used to parse numeric values from the RMI VPTS data. It
#' removes any leading or trailing whitespace and replaces "NaN" with NA.
#'
#' @param x A character vector containing the numeric values to be parsed.
#'
#' @return A numeric vector with the parsed values.
#' @noRd
#'
#' @examples
#' parse_numeric("   42 ")
#' parse_numeric("  -0.775942      ")
#' parse_numeric("nan")
#' parse_numeric("    nan")
parse_numeric <- function(x) {
  string_squish(x) |>
    replace_nan_numeric()
}

#' Parse integer values from RMI VPTS data
#'
#' This function is used to parse integer values from the RMI VPTS data. It
#' removes any leading or trailing whitespace and replaces "NaN" with NA.
#'
#' @param x A character vector containing the integer values to be parsed.
#'
#' @return An integer vector with the parsed values.
#' @noRd
#'
#' @examples
#' parse_integer("   42 ")
#' parse_integer("-4")
#' parse_integer("nan    ")
#' parse_integer("nan")
parse_integer <- function(x) {
  string_squish(x) |>
    replace_nan_numeric() |>
    as.integer()
}

# Function factory to create helpers to parse RMI VPTS --------------------

#' Create a helper function to create helpers to parse RMI VPTS data
#'
#' To simplify `get_vpts_rmi()` we use a helper per field to fetch the
#' information in a vectorised manner.
#'
#' Defining these functions here and loading them into the package environment
#' has the advantage of making it much easier to actually test the generated
#' helpers individually without having to keep track of the column positions
#' in both the test file as well as in `parse_rmi()` in `get_vpts_rmi()`.
#'
#' @param start_value String position where to start reading the value, this is
#'   actually the end position of the previous field as the fwf file is alligned
#'   on the end of the columns.
#' @param stop_value String position where to stop reading the value, this is
#'   actually the start position of the next field as the fwf file is alligned
#'   on the end of the columns.
#' @param parser A function to parse/coerce the value to a R class.
#'
#' @return A function that takes a character vector and returns a parsed value.
#' @noRd
#'
#' @examplesIf interactive()
#' get_datetime <- create_rmi_helper(0, 13, lubridate::ymd_hm)
create_rmi_helper <- function(start_value, stop_value, parser) {
  rmi_helper <- function(lines, start = start_value, stop = stop_value) {
    do.call(parser, list(substr(lines, start, stop)))
  }
  return(rmi_helper)
}

## A list of specifications to create functions from.
specs <- list(
  get_datetime = list(start = 0, stop = 13, parser = lubridate::ymd_hm),
  get_height = list(start = 14, stop = 18, parser = parse_integer),
  get_u = list(start = 19, stop = 25, parser = parse_numeric),
  get_v = list(start = 26, stop = 32, parser = parse_numeric),
  get_w = list(start = 33, stop = 40, parser = parse_numeric),
  get_ff = list(start = 41, stop = 46, parser = parse_numeric),
  get_dd = list(start = 47, stop = 52, parser = parse_numeric),
  get_sd_vvp = list(start = 53, stop = 60, parser = parse_numeric),
  get_gap = list(start = 61, stop = 61, parser = as.logical),
  get_dbz = list(start = 62, stop = 69, parser = parse_numeric),
  get_eta = list(start = 70, stop = 75, parser = parse_numeric),
  get_dens = list(start = 76, stop = 82, parser = parse_numeric),
  get_dbzh = list(start = 83, stop = 90, parser = parse_numeric),
  get_n = list(start = 91, stop = 96, parser = parse_integer),
  get_n_dbz = list(start = 97, stop = 102, parser = parse_integer),
  get_n_all = list(start = 103, stop = 107, parser = parse_integer),
  get_n_dbz_all = list(start = 109, stop = 114, parser = parse_integer)
)

## Actually generate the helper functions
helpers <- purrr::map(
  specs, \(spec){
    do.call(create_rmi_helper, spec)
  }
)

purrr::walk2(names(helpers), helpers, ~ assign(.x, .y, envir = rlang::env_parent()))


# Other RMI helpers -------------------------------------------------------

#' Get the source file name from the RMI VPTS metadata header
#'
#' @param lines A character vector containing the lines of the RMI vpts file.
#'
#' @return A character string representing the source file name.
#' @noRd
#'
#' @examples
#' vroom::vroom_lines("https://opendata.meteo.be/ftp/observations/radar/vbird/bejab/2020/bejab_vpts_20200124.txt") |>
#'   get_rmi_sourcefile()
get_rmi_sourcefile <- function(lines) {
  string_extract(lines, "(?<=input\\: ).+")
}
