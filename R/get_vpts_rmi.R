#' Get VPTS RMI
#' @inheritParams get_vpts_aloft
#' @keywords internal
#'
#' @return A tibble with the parsed VPTS data.
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

  yyyymmdd_in_interval <-
    seq(lubridate::int_start(rounded_interval),
      lubridate::int_end(rounded_interval),
      by = "day"
    ) |>
    format("%Y%m%d")

  rmi_urls <- paste(
    rmi_data_url,
    radar_odim_code,
    years_in_interval,
    paste0(radar_odim_code, "_vpts_", yyyymmdd_in_interval, ".txt"),
    sep = "/"
  )

  # Check if the urls exist, if not, then RMI doens't have data for that
  # radar/datetime combo

  if (purrr::none(rmi_urls, url_exists)) {
    cli::cli_abort(
      "No data found for the requested radar(s) and date(s) on RMI.",
      class = "getRad_error_date_not_found"
    )
  }

  # For every url that exists, parse the VPTS: skip over any days with a missing
  # file
  rmi_files <-
    read_lines_from_url(rmi_urls[purrr::map_lgl(rmi_urls, url_exists)])


  combined_vpts <-
    # drop the header for parsing
    purrr::map(rmi_files, \(lines) tail(lines, -4)) |>
    purrr::map(parse_rmi) |>
    # Add the source_file column
    purrr::map2(rmi_files, ~dplyr::mutate(.x,
                               source_file =
                                 basename(get_rmi_sourcefile(.y)))) |>
    # Add the radar column from the file path
    purrr::map2(rmi_urls, ~dplyr::mutate(.x,
                                   radar = string_extract(.y,
                                                          "(?<=vbird\\/)[a-z]+")
                                   )
                ) |>
    purrr::list_rbind()

  # Enrich with metadata from `weather_radars()`, but only from the `main`
  # source to avoid duplicating rows
  radar_metadata <-
    weather_radars() %>%
    dplyr::filter(.data$source == "main") %>%
    dplyr::mutate(odimcode,
      radar_latitude = latitude,
      radar_longitude = longitude,
      radar_height = heightofstation,
      radar_wavelength = round(
        299792458 / (frequency * 10^7), # speed of light in vacuum
        digits = 1
      ),
      .keep = "none"
    )

  enriched_vpts <-
    dplyr::left_join(combined_vpts,
                     radar_metadata,
                     by = dplyr::join_by(radar == odimcode))

  return(enriched_vpts)
}


#' Parse RMI VPTS data.
#'
#' @param lines A character vector containing the lines of the RMI VPTS file.
#'
#' @return A tibble with the parsed VPTS data.
#' @noRd
#'
#' @examples
#'
#' read_lines_from_url(file.path("https://opendata.meteo.be/",
#'                               "ftp",
#'                               "observations",
#'                               "radar",
#'                               "vbird",
#'                               "bejab",
#'                               "2020",
#'                               "bejab_vpts_20200124.txt")) |>
#'     unlist() |> # read_lines_from_url() returns a list
#'     tail(-4) |> # skip the metadata
#'     parse_rmi()
parse_rmi <- function(lines){
  dplyr::tibble(
    source = "rmi",
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
    dens = get_dens(lines),
    dbzh = get_dbzh(lines),
    n = get_n(lines),
    n_dbz = get_n_dbz(lines),
    n_all = get_n_all(lines),
    n_dbz_all = get_n_dbz(lines)
  )
}
