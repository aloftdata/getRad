

get_datetime <- function(lines, start = 0, stop = 13) {
  substr(lines, start, stop) |>
    lubridate::ymd_hm()
}
get_height <- function(lines, start = 14, stop = 18) {
  substr(lines, start, stop) |>
    parse_integer()
}
get_u <- function(lines, start = 19, stop = 25) {
  substr(lines, start, stop) |>
    parse_numeric()
}
get_v <- function(lines, start = 26, stop = 32) {
  substr(lines, start, stop) |>
    parse_numeric()
}
get_w <- function(lines, start = 33, stop = 40) {
  substr(lines, start, stop) |>
    parse_numeric()
}
get_ff <- function(lines, start = 41, stop = 46) {
  substr(lines, start, stop) |>
    parse_numeric()
}
get_dd <- function(lines, start = 47, stop = 52) {
  substr(lines, start, stop) |>
    parse_numeric()
}
get_sd_vvp <- function(lines, start = 53, stop = 60) {
  substr(lines, start, stop) |>
    parse_numeric()
}
get_gap <- function(lines, start = 61, stop = 61) {
  substr(lines, start, stop) |>
    as.logical()
}
get_dbz <- function(lines, start = 62, stop = 69) {
  substr(lines, start, stop) |>
    parse_numeric()
}
get_eta <- function(lines, start = 70, stop = 75) {
  substr(lines, start, stop) |>
    parse_numeric()
}
get_dens <- function(lines, start = 76, stop = 82) {
  substr(lines, start, stop) |>
    parse_numeric()
}

get_dbzh <- function(lines, start = 83, stop = 90){
  substr(lines, start, stop) |>
    parse_numeric()
}

get_n <- function(lines, start = 91, stop = 96){
  substr(lines, start, stop) |>
    parse_integer()
}

get_n_dbz <- function(lines, start = 97, stop = 102){
  substr(lines, start, stop) |>
    parse_integer()
}

get_n_all <- function(lines, start = 103, stop = 107){
  substr(lines, start, stop) |>
    parse_integer()
}

get_n_dbz_all <- function(lines, start = 109, stop = 114){
  substr(lines, start, stop) |>
    parse_integer()
}

parse_numeric <- function(x) {
  string_squish(x) |>
    replace_nan_numeric()
}

parse_integer <- function(x) {
  string_squish(x) |>
    replace_nan_numeric() |>
    as.integer()
}

#' Get the source file name from the RMI vpts metadata header
#'
#' @param lines A character vector containing the lines of the RMI vpts file.
#'
#' @return A character string representing the source file name.
#' @noRd
#'
#' @examples
#' vroom::vroom_lines("https://opendata.meteo.be/ftp/observations/radar/vbird/bejab/2020/bejab_vpts_20200124.txt") |>
#'      get_rmi_sourcefile()
get_rmi_sourcefile <- function(lines){
  string_extract(lines, "(?<=input\\: ).+")
}
