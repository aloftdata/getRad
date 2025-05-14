#' Get VPTS RMI
#' @inheritParams get_vpts_aloft
#' @keywords internal
#'
#' @return A tibble with the parsed VPTS data.
#'
#' @examplesIf interactive()
#' get_vpts_rmi("bejab",
#'              lubridate::interval("20200119", "20200124"))
get_vpts_rmi <- function(radar_odim_code,
                         rounded_interval) {

  # Build the potential path for the rmi fwf files

  rmi_data_url <- "https://opendata.meteo.be/ftp/observations/radar/vbird"

  rmi_urls <- glue::glue(
    rmi_data_url,
    radar_odim_code,
    "{lubridate::year(year_seq)}",
    "{radar_odim_code}_vpts_{format(year_seq, '%Y%m%d')}.txt",
    year_seq = seq(lubridate::int_start(rounded_interval),
                   lubridate::int_end(rounded_interval),
                   by = "day"
    ),
    .sep = "/"
  )

  ## Check if the urls exist, if not, then RMI doens't have data for that
  ## radar/datetime combo

  if (purrr::none(rmi_urls, url_exists)) {
    cli::cli_abort(
      "No data found for the requested radar(s) and date(s) on RMI.",
      class = "getRad_error_date_not_found"
    )
  }

  ## For every url that exists, parse the VPTS: skip over any days with a
  ## missing file
  resolving_rmi_urls <- rmi_urls[purrr::map_lgl(rmi_urls, url_exists)]
  rmi_files <-
    read_lines_from_url(resolving_rmi_urls)


  combined_vpts <-
    # drop the header for parsing
    purrr::map(rmi_files, \(lines) tail(lines, -4)) |>
    purrr::map(parse_rmi) |>
    # Add the source_file column
    purrr::map2(rmi_files, ~ dplyr::mutate(.x,
      source_file =
        basename(get_rmi_sourcefile(.y))
    )) |>
    # Add the radar column from the file path
    purrr::map2(resolving_rmi_urls, ~ dplyr::mutate(.x,
      radar = string_extract(
        .y,
        "(?<=vbird\\/)[a-z]+"
      )
    )) |>
    purrr::list_rbind()

  # Enrich with metadata from `weather_radars()`, but only from the `main`
  # source to avoid duplicating rows
  radar_metadata <-
    weather_radars() |>
    dplyr::filter(.data$source == "main") |>
    dplyr::mutate(.data$odimcode,
      radar_latitude = .data$latitude,
      radar_longitude = .data$longitude,
      radar_height = .data$heightofstation,
      radar_wavelength = round(
        299792458 / (.data$frequency * 10^7), # speed of light in vacuum
        digits = 1
      ),
      .keep = "none"
    )

  enriched_vpts <-
    dplyr::left_join(combined_vpts,
                     radar_metadata,
                     by = dplyr::join_by("radar" == "odimcode"))

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
parse_rmi <- function(lines) {
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
    n_dbz_all = get_n_dbz(lines),
    sd_vvp_threshold = 2,
    rcs = calc_single_mean_rcs(.data$eta, .data$dens)
  )
}
