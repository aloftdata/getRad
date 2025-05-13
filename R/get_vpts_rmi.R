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

  cols_widths <- c(

  )
  cols$names <- c(
    "date",
    "time",
    "HGHT",
    "u",
    "v",
    "w",
    "ff",
    "dd",
    "sd_vvp",
    "gap",
    "dbz",
    "eta",
    "dens",
    "DBZH",
    "n",
    "n_dbz",
    "n_all",
    "n_dbz_all"
  )
  readr::read_lines("https://opendata.meteo.be/ftp/observations/radar/vbird/bejab/2020/bejab_vpts_20200124.txt") |>
    I() |>
    readr::read_fwf(col_positions = readr::fwf_widths(cols$widths,
                                                      cols$names),
                    comment = "#",
                    show_col_types = FALSE) |>
    purrr::set_names(col_names)

}
