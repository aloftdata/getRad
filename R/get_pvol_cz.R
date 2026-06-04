# http://opendata.chmi.cz/meteorology/weather/radar/sites/ska/vol_z/hdf5/

get_pvol_cz <- function(radar, time, ..., call = rlang::caller_env()) {
  time_chr <- time_pos <- base <- resp <- NULL
  # All parameters are retrieved from separate files
  # Here all urls are generated
  params <- c("z", "u", "v", "w", "zdr", "rhohv", "phidp")
  urls <- glue::glue(
    "http://opendata.chmi.cz/meteorology/weather/radar/sites/{substr(radar,3,5)}/vol_{params}/hdf5/"
  )
  rlang::check_installed(
    c("lubridate", "tidyr", "xml2", "rhdf5"),
    "to read Czech radar data",
    call = call
  )
  res <- lapply(urls, function(x) {
    httr2::request(x) |>
      req_user_agent_getrad() |>
      httr2::req_perform(error_call = call) |>
      httr2::resp_body_html() |>
      xml2::xml_find_all("//a/@href") |>
      xml2::xml_text()
  })
  files_to_get <- data.frame(base = urls) |>
    dplyr::mutate(file = res) |>
    tidyr::unnest(file) |>
    dplyr::filter(file != "../") |>
    dplyr::mutate(
      time_chr = sub(".hdf", "", sub(".*_OKPR_", "", file))
    ) |>
    dplyr::mutate(
      time_pos = strptime(time_chr, "%Y%m%d%H%M%S", tz = "UTC")
    ) |>
    dplyr::filter(lubridate::`%within%`(
      time_pos,
      lubridate::interval(
        time,
        time + lubridate::minutes(5)
      )
    )) |>
    dplyr::mutate(url = paste0(base, file))
  # There are sometimes multiple pvols, generally one with 12 scans
  # additionally there are (sometimes) scans in separate files for
  # the 0.3 and 1.5 elevation angle. These are not always both present.
  # It all seems a bit irregular
  pvols <- lapply(
    split(files_to_get$url, files_to_get$time_chr),
    read_pvol_from_url_per_param
  )
  if (length(pvols) == 1) {
    return(pvols[[1]])
  }
  attr <- purrr::map(pvols, purrr::pluck, "attributes") |>
    purrr::map(purrr::assign_in, where = c("what", "time"), purrr::zap()) |>
    purrr::map(purrr::assign_in, where = c("what", "date"), purrr::zap()) |>
    purrr::map(purrr::assign_in, where = c("how", "scan_count"), purrr::zap())
  if (!all(unlist(lapply(attr[-1], identical, attr[[1]])))) {
    cli::cli_abort(
      c(
        "The attributes of the polar volumes about to be merged differ"
      ),
      class = "getRad_error_czechia_attributes_differ",
      call = call
    )
  }
  pvol <- purrr::pluck(pvols, 1)
  pvol$scans <- unlist(recursive = F, lapply(pvols, purrr::pluck, 'scans'))
  pvol$attributes$how$scan_count <- length(pvol$scans)
  pvol$datetime <- max(do.call(c, lapply(pvols, purrr::pluck, "datetime")))
  pvol$attributes$what$time <- max(unlist(
    (lapply(pvols, \(x) x$attributes$what$time))
  ))
  pvol$attributes$what$date <- max(unlist(
    (lapply(pvols, \(x) x$attributes$what$date))
  ))
  if (anyDuplicated(get_elevation_angles(pvol))) {
    # Note that if scanning pattern changes this might flag false positive, but
    # currently the check is there to prevent falsely merging scans from two iterations
    # of the scanning pattern
    cli::cli_abort(
      c(
        "There are duplicated elevation angles, likely as a result of merging the wrong scans."
      ),
      class = "getRad_error_czechia_duplicated_elevation_angles",
      call = call
    )
  }
  return(pvol)
}
