get_pvol_us <- function(radar, time, ...) {

    rlang::check_installed(
    c("xml2", "lubridate", "tidyr", "httr2"),
    "to import data from US weather radars"
  )
  
  if (!inherits(time, "POSIXct"))
    cli::cli_abort("{.arg time} must be POSIXct.", class = "getRad_error_us_time_not_posix")

  key <- .nearest_nexrad_key(time, radar)
  url <- paste0("https://noaa-nexrad-level2.s3.amazonaws.com/", key)

  tmp <- tempfile(fileext = ".h5")
  tryCatch(
    httr2::request(url)             |>
      req_user_agent_getrad()       |>
      httr2::req_perform(path = tmp),
    httr2_http_404 = function(cnd)
      cli::cli_abort("NEXRAD file not found at {.url {url}}.",
                     cnd = cnd, class = "getRad_error_us_file_not_found")
  )

  pvol <- bioRad::read_pvolfile(tmp, ...)
  unlink(tmp)
  pvol
}


#' @noRd
.list_nexrad_keys <- function(date, radar) {
  d       <- as.Date(date, tz = "UTC")
  prefix  <- sprintf("%04d/%02d/%02d/%s/", lubridate::year(d),
                     lubridate::month(d), lubridate::day(d), toupper(radar))
  ns      <- c(s3 = "http://s3.amazonaws.com/doc/2006-03-01/")
  host    <- "https://noaa-nexrad-level2.s3.amazonaws.com"
  keys    <- character()
  token   <- NULL

  repeat {
    xml <- httr2::request(host) |>
      httr2::req_url_query(`list-type` = "2",
                           prefix     = prefix,
                           `continuation-token` = token) |>
      httr2::req_perform() |>
      httr2::resp_body_xml()

    keys  <- c(keys, xml2::xml_text(xml2::xml_find_all(xml, ".//s3:Key", ns)))
    if (xml2::xml_text(xml2::xml_find_first(xml, ".//s3:IsTruncated", ns)) == "false")
      break
    token <- xml2::xml_text(xml2::xml_find_first(xml, ".//s3:NextContinuationToken", ns))
  }
  keys
}



#' @noRd
.nearest_nexrad_key <- function(datetime, radar) {
  days <- unique(as.Date(datetime + c(-86400, 0, 86400), tz = "UTC"))
  keys <- unlist(lapply(days, .list_nexrad_keys, radar = radar), use.names = FALSE)

  keys <- keys[!grepl("_MDM(\\.gz)?$", keys)]                # drop metadata
  ts   <- lubridate::ymd_hms(sub(".*([0-9]{8}_[0-9]{6}).*", "\\1", keys),
                             tz = "UTC", quiet = TRUE)

  prior <- which(ts <= datetime)
  if (!length(prior))
    cli::cli_abort("No earlier scan found for {.val {radar}} at that time.",
                   class = "getRad_error_us_no_prior_scan")

  keys[prior[which.max(ts[prior])]]
}




nexrad_key_to_url <- function(key) {
  paste0("https://noaa-nexrad-level2.s3.amazonaws.com/", key)
}

target_time <- ymd_hms("2024-05-12 04:10:00", tz = "UTC")
key   <- nearest_nexrad_key(target_time, "KABR")
url   <- nexrad_key_to_url(key)
