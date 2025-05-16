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

# Function factory to create helpers to parse RMI VPTS --------------------

#' Create a helper function to create helpers to parse RMI VPTS data
#'
#' To simplify `get_vpts_rmi()` we use a helper per field to fetch the
#' information in a vectorised manner.
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

# Helper to actually parse a RMI fwf VPTS file ----------------------------

#' Parse RMI VPTS data.
#'
#' This function parses the RMI VPTS data from a character vector containing
#' the lines of the file.
#'
#' Internally this function generates a number of helper functions to parse the
#' different fields in the VPTS data. The helper functions are generated
#' using the `create_rmi_helper()` function based on the specifications
#' provided in the `specs` list.
#'
#' @param lines A character vector containing the lines of the RMI VPTS file.
#'
#' @return A tibble with the parsed VPTS data.
#' @noRd
#'
#' @examples
#'
#' read_lines_from_url(file.path(
#'   "https://opendata.meteo.be/",
#'   "ftp",
#'   "observations",
#'   "radar",
#'   "vbird",
#'   "bejab",
#'   "2020",
#'   "bejab_vpts_20200124.txt"
#' )) |>
#'   unlist() |> # read_lines_from_url() returns a list
#'   tail(-4) |> # skip the metadata
#'   parse_rmi()
parse_rmi <- function(lines) {
  ## A list of specifications to create helper functions from. This is where the
  ## column locations within the fwf file are defined.
  specs <- list(
    get_datetime = list(start = 0, stop = 13, parser = lubridate::ymd_hm),
    get_height = list(start = 14, stop = 18, parser = parse_numeric),
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
    get_n = list(start = 91, stop = 96, parser = parse_numeric),
    get_n_dbz = list(start = 97, stop = 102, parser = parse_numeric),
    get_n_all = list(start = 103, stop = 107, parser = parse_numeric),
    get_n_dbz_all = list(start = 109, stop = 114, parser = parse_numeric)
  )

  with(
    # With this list of helper functions:
    purrr::map(
      specs, \(spec){
        do.call(create_rmi_helper, spec)
      }
    ),
    # Run this chunk (and use the helpers to parse the fwf file):
    {
      dplyr::tibble(
        source = "rmi",
        datetime = get_datetime(lines),
        height = get_height(lines),
        u = get_u(lines),
        v = get_v(lines),
        w = get_w(lines),
        ff = get_ff(lines),
        dd = get_dd(lines),
        sd_vvp = get_sd_vvp(lines),
        gap = get_gap(lines),
        dbz = get_dbz(lines),
        eta = get_eta(lines),
        dens = get_dens(lines),
        dbzh = get_dbzh(lines),
        n = get_n(lines),
        n_dbz = get_n_dbz(lines),
        n_all = get_n_all(lines),
        n_dbz_all = get_n_dbz_all(lines),
        sd_vvp_threshold = 2,
        rcs = calc_single_mean_rcs(.data$eta, .data$dens)
      )
    }
  )
}

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
