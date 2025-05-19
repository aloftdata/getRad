#' Get weather radar metadata
#'
#' Gets weather radar metadata from [OPERA](
#' https://www.eumetnet.eu/activities/observations-programme/current-activities/opera/)
#' and/or [NEXRAD](
#' https://www.ncei.noaa.gov/products/radar/next-generation-weather-radar).
#'
#' @details
#' The source files for this function are:
#' - For `opera`: [OPERA_RADARS_DB.json](
#' http://eumetnet.eu/wp-content/themes/aeron-child/observations-programme/current-activities/opera/database/OPERA_Database/OPERA_RADARS_DB.json)
#' and [OPERA_RADARS_ARH_DB.json](
#' http://eumetnet.eu/wp-content/themes/aeron-child/observations-programme/current-activities/opera/database/OPERA_Database/OPERA_RADARS_ARH_DB.json).
#' - For `nexrad`: [nexrad-stations.txt](https://www.ncei.noaa.gov/access/homr/file/nexrad-stations.txt).
#'
#' @inheritParams req_cache_getrad
#' @param source Source of the metadata. `"opera"`, `"nexrad"` or `"all"`.
#' @param ... Additional arguments passed on to reading functions per source,
#'   currently not used.
#' @return A tibble with weather radar metadata. In all cases the column `source` is
#' added to indicate the source of the data and `radar` to show the radar identifiers
#'  used in other functions like [get_pvol()] and [get_vpts()].
#' @export
#' @examplesIf interactive()
#' # Get radar metadata from OPERA
#' weather_radars(source = "opera")
#'
#' # Get radar metadata from NEXRAD
#' weather_radars(source = "nexrad")
weather_radars <- function(source = c("opera"), use_cache = TRUE, ...) {
  if (!rlang::is_character(source) || any(is.na(source)) || length(source) == 0) {
    cli::cli_abort("{.arg source} is not valid, it should be an {.cls character}
                   vector with a length of atleast one not contain NA values.",
      class = "getRad_error_weather_radar_source_not_character"
    )
  }
  valid_source_options <- c("opera", "nexrad")
  if ("all" %in% source) {
    source <- valid_source_options
  }
  if (!all(s <- source %in% valid_source_options)) {
    cli::cli_abort(
      c(
        x = "{.val {source[!s]}} {?is not a/are not} valid option{?s} for the {.arg source} argument.",
        i = "{.val {valid_source_options}} are possible valid sources."
      ),
      class = "getRad_error_weather_radar_source_not_valid"
    )
  }
  if (!rlang::is_scalar_character(source)) {
    t <- purrr::map(source, ~ weather_radars(source = .x, use_cache = use_cache, ...)) |> dplyr::bind_rows()
    return(t)
  }
  switch(source,
    "opera" = weather_radars_opera(use_cache = use_cache, ...),
    "nexrad" = weather_radars_nexrad(use_cache = use_cache, ...)
  ) |> dplyr::mutate(source = source)
}
weather_radars_opera <- function(use_cache = TRUE, ...,
                                 call = rlang::caller_env()) {
  # Build the url where the JSON files are hosted on eumetnet

  # Read source JSON files from OPERA
  radars_main_url <-
    paste(
      sep = "/",
      "http://eumetnet.eu/wp-content/themes/aeron-child",
      "observations-programme/current-activities/opera/database",
      "OPERA_Database/OPERA_RADARS_DB.json"
    )

  radars_archive_url <-
    paste(
      sep = "/",
      "http://eumetnet.eu/wp-content/themes/aeron-child",
      "observations-programme/current-activities/opera/database",
      "OPERA_Database/OPERA_RADARS_ARH_DB.json"
    )

  urls <- list(
    c(url = radars_main_url, origin = "main"),
    c(url = radars_archive_url, origin = "archive")
  )

  # Fetch the JSON file from eumetnet with similar arguments as the other
  # functions
  purrr::map(urls, \(json_url) {
    httr2::request(json_url["url"]) |>
      req_user_agent_getrad() |>
      req_retry_getrad() |>
      req_cache_getrad(use_cache = use_cache) |>
      httr2::req_perform(error_call = call) |>
      # The object is actually returned as text/plain
      httr2::resp_body_json(check_type = FALSE) |>
      # As tibble so it displays more nicely
      purrr::map(\(list) dplyr::as_tibble(list)) |>
      # Return as a single tibble by row binding
      purrr::list_rbind() |>
      dplyr::mutate(origin = json_url["origin"])
  }) |>
    # Combine both sources into a single tibble
    purrr::list_rbind() |>
    # Convert empty strings into NA
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.character),
        \(string) dplyr::if_else(string == "",
          NA_character_,
          string
        )
      )
    ) |>
    # Move source column to end
    dplyr::relocate("origin", .after = dplyr::last_col()) |>
    # convert column types to expected values, non fitting values are returned
    # as NA without warning
    dplyr::mutate(
      number = as_integer_shh(.data$number),
      wmocode = as_integer_shh(.data$wmocode),
      status = as_integer_shh(.data$status),
      latitude = as_numeric_shh(.data$latitude),
      longitude = as_numeric_shh(.data$longitude),
      heightofstation = as_integer_shh(.data$heightofstation),
      doppler = yes_no_as_logical(.data$doppler),
      maxrange = as_integer_shh(.data$maxrange),
      startyear = as_integer_shh(.data$startyear), ,
      heightantenna = as_numeric_shh(.data$heightantenna),
      diameterantenna = as_numeric_shh(.data$diameterantenna),
      beam = as_numeric_shh(.data$beam),
      gain = as_numeric_shh(.data$gain),
      frequency = as_numeric_shh(.data$frequency),
      wrwp = yes_no_as_logical(.data$wrwp),
      finishyear = as_integer_shh(.data$finishyear),
      singlerrr = yes_no_as_logical(.data$singlerrr),
      compositerrr = yes_no_as_logical(.data$compositerrr),
      radar = .data$odimcode
    ) |>
    dplyr::select("radar", dplyr::everything()) |>
    # Sort data for consistent git diffs
    dplyr::arrange(.data$country, .data$number, .data$startyear)
}

weather_radars_nexrad <- function(use_cache = TRUE, ...,
                                  call = rlang::caller_env()) {
  #  https://www.ncei.noaa.gov/access/homr/reports
  file_content <- httr2::request("https://www.ncei.noaa.gov/access/homr/file/nexrad-stations.txt") |>
    req_user_agent_getrad() |>
    req_cache_getrad(use_cache = TRUE) |>
    httr2::req_perform(error_call = call) |>
    httr2::resp_body_string()
  # First parse first lines to find column widths and headers
  tmp <- file_content |>
    I() |>
    vroom::vroom_fwf(show_col_types = F, n_max = 2)

  widths <- vroom::fwf_widths(nchar(unlist(tmp[2, ])) + 1, tolower(unlist(tmp[1, ])))
  # for type specification see: https://www.ncei.noaa.gov/access/homr/file/NexRad_Table.txt
  file_content |>
    I() |>
    vroom::vroom_fwf(
      show_col_types = F, col_positions = widths, skip = 2,
      col_types = vroom::cols(
        ncdcid = "i", icao = "c", wban = "c", name = "c",
        country = "c", st = "c", county = "c", lat = "d",
        lon = "d", elev = "i", utc = "i", stntype = "c"
      )
    ) |>
    dplyr::mutate(
      radar = .data$icao,
      latitude = .data$lat,
      longitude = .data$lon,
      country = capwords(tolower(.data$country)),
      location = capwords(sub(
        " wfo", " WFO",
        sub(
          " ab", " AB",
          sub(
            " faa", " FAA",
            sub(" jfk", " JFK", sub(" afb", " AFB", tolower(.data$name)))
          )
        )
      )),
      heightantenna = .data$elev / 3.28083989
    ) |>
    dplyr::select(-dplyr::all_of(c("lat", "lon"))) |>
    dplyr::select("radar", dplyr::everything())
}

# from base::chartr examples
capwords <- function(s, strict = FALSE) {
  cap <- function(s) {
    paste(toupper(substring(s, 1, 1)),
      {
        s <- substring(s, 2)
        if (strict) tolower(s) else s
      },
      sep = "",
      collapse = " "
    )
  }
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
