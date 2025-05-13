#' Get VPTS RMI
#'
#' @param radar_odim_code
#' @param rounded_interval
#'
#' @return
#' @export
#'
#' @examples
#' get_vpts_rmi("bejab",
#'              lubridate::interval("20200119", "20200124"))
get_vpts_rmi <- function(radar_odim_code,
                         rounded_interval) {

  ## Build the potential path for the rmi fwf

  rmi_data_url <- "https://opendata.meteo.be/ftp/observations/radar/vbird/"

  ## extract the years in the interval
  years_in_interval <-
    seq(lubridate::year(lubridate::int_start(rounded_interval)),
        lubridate::year(lubridate::int_end(rounded_interval)))


  paste(rmi_data_url, radar_odim_code, years_in_interval)

  yyyymmdd_in_interval <-
    seq(lubridate::int_start(rounded_interval),
      lubridate::int_end(rounded_interval),
      by = "day"
    ) |>
    format("%Y%m%d")

  rmi_urls <- glue::glue(rmi_data_url,
    radar_odim_code,
    years_in_interval,
    "{radar_odim_code}_vpts_{yyyymmdd_in_interval}.txt",
    .sep = "/"
  )

  # For every url, parse the VPTS
  purrr::map(rmi_urls, readr::read_lines) |>
    purrr::map(parse_rmi) |>
    purrr::list_rbind() |>
    return()

  # cols_widths <- c(
  #
  # )
  # cols$names <- c(
  #   "date",
  #   "time",
  #   "HGHT",
  #   "u",
  #   "v",
  #   "w",
  #   "ff",
  #   "dd",
  #   "sd_vvp",
  #   "gap",
  #   "dbz",
  #   "eta",
  #   "dens",
  #   "DBZH",
  #   "n",
  #   "n_dbz",
  #   "n_all",
  #   "n_dbz_all"
  # )
  # readr::read_lines("https://opendata.meteo.be/ftp/observations/radar/vbird/bejab/2020/bejab_vpts_20200124.txt") |>
  #   I() |>
  #   readr::read_fwf(col_positions = readr::fwf_widths(cols$widths,
  #                                                     cols$names),
  #                   comment = "#",
  #                   show_col_types = FALSE) |>
  #   purrr::set_names(col_names)

}
# fwf_text <-
#   readr::read_lines("https://opendata.meteo.be/ftp/observations/radar/vbird/bejab/2020/bejab_vpts_20200124.txt")
# row_index <- 5
# get_datetime <- function(row_index, start = 0, stop = 13){
#   purrr::chuck(fwf_text, row_index) |>
#     substr(start, stop) |>
#     lubridate::ymd_hm()
# }
# get_height <- function(row_index, start = 14, stop = 18){
#   purrr::chuck(fwf_text, row_index) |>
#     substr(start, stop) |>
#     string_squish() |>
#     as.integer()
# }
# get_u
# get_v
# get_w
# get_ff <- function(row, 41, 46)
# get_dd <- function(row, 47, 52)
# get_sd_vpp <- function(row, 41,46)
# get_gap
# get_dbz
# get_eta <- function(row, 70, 75)
# get_dens <- function(row, 76, 82)

parse_rmi <- function(lines){
  dplyr::tibble(
    datetime = get_datetime(lines),
    height = get_height(lines),
    u = get_u(lines),
    v = get_v(lines),
    w = get_w(lines),
    ff = get_ff(lines),
    dd = get_dd(lines),
    sd_vpp = get_sd_vpp(lines),
    gap = get_gap(lines),
    dbz = get_dbz(lines),
    eta = get_eta(lines),
    dens = get_dens(lines)
  )
}
