#' Get VPTS RMITitle
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
}
