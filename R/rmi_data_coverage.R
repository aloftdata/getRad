#' Get RMI data coverage
#'
#' This function retrieves the RMI data coverage for a given radar and year.
#'
#' @param radar Optional. Character vector of radars to get coverage for.
#' @param year Optional. Integer vector of years to get coverage for.
#' @return A tibble with RMI data coverage.
#' @noRd
#' @examplesIf interactive()
#' # Get coverage for all radars and years
#' rmi_data_coverage()
#'
#' # For a single radar, for a few years
#' rmi_data_coverage(radar = "behel", year = c(2020, 2021))
#'
#' # For several radars for a single year
#' rmi_data_coverage(radar = c("frave", "bezav", "nlhrw"), year = 2024)
rmi_data_coverage <- function(radar = NULL, year = NULL, ..., call = rlang::caller_env()) {
  rlang::check_installed(
    c("tidyr"),
    "to read coverage date from RMI",
    call = call
  )

  base_url <-
    "https://opendata.meteo.be/ftp/observations/radar/vbird"

  found_radars <- get_element_regex(get_html(base_url, call = call), "[a-z]{5}(?=\\/)")

  if (missing(radar)) {
    radar <- found_radars
  }

  if (missing(year)) {
    use_year_filter <- FALSE
  } else {
    use_year_filter <- TRUE
  }

  if (any(!radar %in% found_radars)) {
    cli::cli_abort("Requested radar {radar[!radar %in% found_radars]} not
                   present in RMI coverage", call = call)
  }

  radar_year_combos <-
    purrr::map(
      found_radars,
      \(radar) get_element_regex(
        get_html(file.path(base_url, radar), call = call),
        "[0-9]{4}"
      )
    ) |>
    purrr::set_names(found_radars)

  years_covered_by_rmi <- as.integer(unique(unlist(radar_year_combos)))

  if (!all(year %in% years_covered_by_rmi) && use_year_filter) {
    cli::cli_abort("Requested year {year[!year %in% years_covered_by_rmi]}
     not present in RMI coverage",
      class = "getRad_error_date_not_found", call = call
    )
  }

  radar_year_combos |>
    # Only keep the radars in the radar argument
    (\(list) list[radar])() |>
    # Only keep the years in the year argument
    (\(years_per_radar) {
      if (use_year_filter) {
        purrr::map(
          years_per_radar,
          \(radar_years) radar_years[radar_years %in% year]
        )
      } else {
        years_per_radar
      }
    })() |>
    (\(years_per_radar)    {
      purrr::map2(
        years_per_radar,
        names(years_per_radar),
        ~ file.path(base_url, .y, .x)
      )
    })() |>
    purrr::map(\(year_url) {
      purrr::map(year_url, ~ get_element_regex(
        get_html(.x, call = call),
        "[a-z]{5}_vpts_.+"
      )) |>
        purrr::set_names(basename(year_url))
    }) |>
    tibble::enframe(name = "radar", value = "years") |>
    tidyr::unnest_wider(col = "years") |>
    tidyr::pivot_longer(
      cols = -"radar",
      names_to = "year",
      values_to = "file"
    ) |>
    tidyr::unnest_longer("file") |>
    dplyr::mutate(
      file = unlist(file),
      date = lubridate::ymd(string_extract(file, "[0-9]{8}(?=\\.txt)")),
      directory = file.path(
        string_replace(base_url, ".+(?<=\\/ftp)", ""),
        radar,
        year
      )
    ) |>
    dplyr::select("directory", "file", "radar", "date")
}
