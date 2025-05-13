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
get_u
get_v
get_w
get_ff <- function(row_index, start = 41, stop = 46){

}
get_dd <- function(row_index, start = 47, stop = 52){}
get_sd_vpp <- function(row_index, start = 41,stop = 46){}
get_gap
get_dbz
get_eta <- function(row_index, start = 70, stop = 75){}
get_dens <- function(row_index, start = 76, stop = 82){}
