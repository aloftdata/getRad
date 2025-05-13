# All helpers should call this to parse "nan" strings.
replace_nan <- function(string){
  if(string == "nan"){return(NaN)}
}

get_datetime <- function(row_index, start = 0, stop = 13){
  purrr::chuck(fwf_text, row_index) |>
    substr(start, stop) |>
    lubridate::ymd_hm()
}
get_height <- function(row_index, start = 14, stop = 18){
  purrr::chuck(fwf_text, row_index) |>
    substr(start, stop) |>
    string_squish() |>
    as.integer()
}
get_u <- function(row_index, start = 19, stop = 25){
  purrr::chuck(fwf_text, row_index) |>
    substr(start, stop) |>
    parse_numeric()
}
get_v <- function(row_index, start = 26, stop = 32){
  purrr::chuck(fwf_text, row_index) |>
    substr(start, stop) |>
    parse_numeric()
}
get_w <- function(row_index, start = 33, stop = 40){
  purrr::chuck(fwf_text, row_index) |>
    substr(start, stop) |>
    parse_numeric()
}
get_ff <- function(row_index, start = 41, stop = 46){
  purrr::chuck(fwf_text, row_index) |>
    substr(start, stop) |>
    parse_numeric()
}
get_dd <- function(row_index, start = 47, stop = 52){
  purrr::chuck(fwf_text, row_index) |>
    substr(start, stop) |>
    parse_numeric()
}
get_sd_vpp <- function(row_index, start = 41,stop = 46){
  purrr::chuck(fwf_text, row_index) |>
    substr(start, stop) |>
    parse_numeric()
}
get_gap <- function(row_index, start = 61, stop = 61){
  purrr::chuck(fwf_text, row_index) |>
    substr(start, stop) |>
    as.logical()
}
get_dbz <- function(row_index, start = 62, stop = 69){
  purrr::chuck(fwf_text, row_index) |>
    substr(start, stop) |>
    parse_numeric()
}
get_eta <- function(row_index, start = 70, stop = 75){
  purrr::chuck(fwf_text, row_index) |>
    substr(start, stop) |>
    parse_numeric()
}
get_dens <- function(row_index, start = 76, stop = 82){
  purrr::chuck(fwf_text, row_index) |>
    substr(start, stop) |>
    parse_numeric()
}

parse_numeric <- function(x){
  string_squish(x) |>
    as.numeric()
}
