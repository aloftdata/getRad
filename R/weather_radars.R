#' Get weather radar metadata
#'
#' Gets weather radar metadata from [OPERA](
#' https://www.eumetnet.eu/activities/observations-programme/current-activities/opera/).
#'
#' The source files for this function are:
#' - For `opera`: [OPERA_RADARS_DB.json](
#' http://eumetnet.eu/wp-content/themes/aeron-child/observations-programme/current-activities/opera/database/OPERA_Database/OPERA_RADARS_DB.json)
#' and [OPERA_RADARS_ARH_DB.json](
#' http://eumetnet.eu/wp-content/themes/aeron-child/observations-programme/current-activities/opera/database/OPERA_Database/OPERA_RADARS_ARH_DB.json).
#'
#' @inheritParams req_cache_getrad
#' @return A tibble with weather radar metadata.
#' @export
#' @examplesIf interactive()
#' weather_radars()
weather_radars <- function(use_cache = TRUE) {

}
weather_radars_opera <- function(use_cache = TRUE) {
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
    c(url = radars_main_url, source = "main"),
    c(url =    radars_archive_url, source = "archive")
  )

  # Fetch the JSON file from eumetnet with similar arguments as the other
  # functions
  purrr::map(urls, \(json_url) {
    httr2::request(json_url["url"]) |>
      req_user_agent_getrad() |>
      req_retry_getrad() |>
      req_cache_getrad(use_cache = use_cache) |>
      httr2::req_perform() |>
      # The object is actually returned as text/plain
      httr2::resp_body_json(check_type = FALSE) |>
      # As tibble so it displays more nicely
      purrr::map(\(list) dplyr::as_tibble(list)) |>
      # Return as a single tibble by row binding
      purrr::list_rbind() |>
      dplyr::mutate(source = json_url["source"])
  }) |>
    # Combine both sources into a single tibble
    purrr::list_rbind() |>
    # Convert empty strings into NA
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character),
      \(string) dplyr::if_else(string == "",
                               NA_character_,
                               string)
        )
      ) |>
    # Move source column to end
    dplyr::relocate(source, .after = dplyr::last_col()) |>
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
      startyear = as_integer_shh(.data$startyear),,
      heightantenna = as_numeric_shh(.data$heightantenna),
      diameterantenna = as_numeric_shh(.data$diameterantenna),
      beam = as_numeric_shh(.data$beam),
      gain = as_numeric_shh(.data$gain),
      frequency = as_numeric_shh(.data$frequency),
      wrwp = yes_no_as_logical(.data$wrwp),
      finishyear = as_integer_shh(.data$finishyear),
      singlerrr = yes_no_as_logical(.data$singlerrr),
      compositerrr = yes_no_as_logical(.data$compositerrr)
    ) |>
    # Sort data for consistent git diffs
    dplyr::arrange(.data$country, .data$number, .data$startyear)
}

weather_radars_nexrad<-function(use_cache = TRUE,...)
{
  #  https://www.ncei.noaa.gov/access/homr/reports
  file_content<-httr2::request('https://www.ncei.noaa.gov/access/homr/file/nexrad-stations.txt') |>
    req_user_agent_getrad() |> req_cache_getrad(use_cache = TRUE)|>
    httr2::req_perform() |> httr2::resp_body_string()

  tmp<-file_content|>
    I()|>
    vroom::vroom_fwf(show_col_types = F, n_max = 2)

  widths<-vroom::fwf_widths(nchar(unlist(tmp[2,]))+1, tolower(unlist(tmp[1,])))
# for type specification see: https://www.ncei.noaa.gov/access/homr/file/NexRad_Table.txt
  file_content|>
    I()|>
    vroom::vroom_fwf(show_col_types = F, col_positions = widths,skip = 2,
                      col_types = vroom::cols(
                        ncdcid='i',icao='c', wban='c', name='c',
                        country='c',st='c',county='c', lat='d', lon='d', elev='i',utc='i', stntype='c'
                      ))
 }
