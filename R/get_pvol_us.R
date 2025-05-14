.nexrad_cache <- new.env(parent = emptyenv()) #initiate cache

get_pvol_us <- function(radar, time, ...) {

  if (!inherits(time, "POSIXct"))
    cli::cli_abort("{.arg time} must be POSIXct.", class = "getRad_error_us_time_not_posix")

  key <- .nearest_nexrad_key(time, radar)
    if (exists(key, envir = .nexrad_cache, inherits = FALSE)) {
    return(.nexrad_cache[[key]])
  }
  url <- paste0("https://noaa-nexrad-level2.s3.amazonaws.com/", key)
  tmp <- file.path(tempdir(), basename(key))

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
  .nexrad_cache[[key]] <- pvol  #cache keys to avoid duplicate downloads
  pvol
}

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
                           prefix = prefix,
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

.nearest_nexrad_key <- function(datetime, radar) {
  days <- unique(as.Date(datetime + c(-86400, 0, 86400), tz = "UTC"))
  keys <- unlist(lapply(days, .list_nexrad_keys, radar = radar), use.names = FALSE)

  keys <- keys[!grepl("_MDM(\\.gz)?$", keys)]
  ts   <- lubridate::ymd_hms(sub(".*([0-9]{8}_[0-9]{6}).*", "\\1", keys),
                             tz = "UTC", quiet = TRUE)
  if (!length(ts)) {
    cli::cli_abort(
      "No scans found for {.val {radar}} near {.val {format(datetime, '%F %T %Z')}}",
      class = "getRad_error_us_no_scan_found"
    )
  }
  keys[which.min(abs(difftime(ts, datetime, units = "secs")))]
}

nexrad_key_to_url <- function(key) {
  paste0("https://noaa-nexrad-level2.s3.amazonaws.com/", key)
}

target_time <- lubridate::ymd_hms("2024-05-12 04:10:00", tz = "UTC")
key   <- .nearest_nexrad_key(target_time, "KABR")
url   <- nexrad_key_to_url(key)
print(url)
