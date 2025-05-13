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

  rmi_data_url <- "https://opendata.meteo.be/ftp/observations/radar/vbird"

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
  combined_vpts <-
    purrr::map(rmi_urls, vroom::vroom_lines) |>
    purrr::map(parse_rmi) |>
    purrr::list_rbind()

  return(combined_vpts)
}


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
