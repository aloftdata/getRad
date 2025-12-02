get_pvol_us <- function(radar, datetime, ..., call = rlang::caller_env()) {
  if (!inherits(datetime, c("Interval", "POSIXct"))) {
    cli::cli_abort(
      "{.arg datetime} must be {.cls POSIXct} or {.cls Interval}.",
      class = "getRad_error_us_time_not_posix",
      call = call
    )
  }
  keys <- .most_representative_nexrad_key(datetime, radar, call = call)
  pvol <- list()
  for (key in keys) {
    url <- nexrad_key_to_url(key)

    pvol[[key]] <- withr::with_tempfile("file", pattern = basename(url), {
      tryCatch(
        httr2::request(url) |>
          req_user_agent_getrad() |>
          httr2::req_perform(path = file, error_call = call),
        httr2_http_404 = function(cnd) {
          cli::cli_abort(
            "Can't find NEXRAD file at {.url {url}}.",
            call = call,
            cnd = cnd,
            class = "getRad_error_us_file_not_found"
          )
        }
      )
      bioRad::read_pvolfile(file, ...)
    })
  }
  if (!lubridate::is.interval(datetime)) {
    pvol <- pvol[[1]]
  }
  pvol
}

#' List next nexrad keys for a a vector of dates
#'
#' @param date A date of length one
#' @param radar A scalar character with the radar key
#'
#' @returns a vector as keys as a character string
#'
#' @noRd
#' @examples
#' .list_nexrad_keys(as.Date("2025-3-4"), "KARX")
.list_nexrad_keys <- function(date, radar, ..., call = rlang::caller_env()) {
  d <- as.Date(date, tz = "UTC")
  if (!rlang::is_scalar_character(radar)) {
    cli::cli_abort(
      "{.arg radar} must be single radar code.",
      class = "getRad_error_pvol_us_radar_not_scalar",
      call = call
    )
  }
  prefix <- sprintf(
    "%04d/%02d/%02d/%s/",
    lubridate::year(d),
    lubridate::month(d),
    lubridate::day(d),
    toupper(radar)
  )
  ns <- c(s3 = "http://s3.amazonaws.com/doc/2006-03-01/")
  host <- getOption(
    "getRad.nexrad_data_url",
    default = "https://unidata-nexrad-level2.s3.amazonaws.com"
  )
  keys <- character()
  token <- NULL
  cache <- getOption("getRad.cache")
  cache_key <- tolower(glue::glue(
    "list_nexrad_keys_{radar}_{d}_{
                        ifelse((d>=(Sys.Date()-1)),
                        lubridate::floor_date(Sys.time(),'5 mins'),'historic')}"
  ))
  if (cache$exists(cache_key)) {
    return(cache$get(cache_key))
  }
  repeat {
    xml <- httr2::request(host) |>
      req_user_agent_getrad() |>
      httr2::req_url_query(
        `list-type` = "2",
        prefix = prefix,
        `continuation-token` = token
      ) |>
      httr2::req_perform(error_call = call) |>
      httr2::resp_body_xml()

    keys <- c(keys, xml2::xml_text(xml2::xml_find_all(xml, ".//s3:Key", ns)))
    if (
      xml2::xml_text(xml2::xml_find_first(xml, ".//s3:IsTruncated", ns)) ==
        "false"
    ) {
      break
    }
    token <- xml2::xml_text(xml2::xml_find_first(
      xml,
      ".//s3:NextContinuationToken",
      ns
    ))
  }
  cache$set(key = cache_key, value = keys)
  keys
}

#' Fine the most representative key for a timestamps radar combination within the nexrad network
#'
#' @param datetime a POSIXct datetime or Interval of length one
#' @param radar A radar of length one
#'
#' @returns a character with the name of the key(s)
#'
#' @noRd
#' @examples
#' .most_representative_nexrad_key(lubridate::as_datetime("2024-5-9 14:44:00"), "KBBX")
.most_representative_nexrad_key <- function(
  datetime,
  radar,
  call = rlang::caller_env()
) {
  if (lubridate::is.interval(datetime)) {
    days <- unique(as.Date(
      seq(
        lubridate::int_start(datetime) - lubridate::days(1),
        lubridate::int_start(datetime) + lubridate::days(2),
        "day"
      ),
      tz = "UTC"
    ))
  } else {
    days <- unique(as.Date(datetime + c(-86400, 0, 86400), tz = "UTC"))
  }
  keys <- unlist(
    lapply(days, .list_nexrad_keys, radar = radar, call = call),
    use.names = FALSE
  )

  keys <- keys[!grepl("_MDM(\\.gz)?$", keys)]
  ts <- lubridate::ymd_hms(
    sub(".*([0-9]{8}_[0-9]{6}).*", "\\1", keys),
    tz = "UTC",
    quiet = TRUE
  )
  if (!length(ts)) {
    cli::cli_abort(
      "Can't find scans for radar {.val {radar}} near {.val {format(datetime,
       \"%F %T %Z\")}}.",
      class = "getRad_error_us_no_scan_found",
      call = call
    )
  }
  if (lubridate::is.interval(datetime)) {
    keys[ts %within% datetime]
  } else {
    keys[max(which(ts <= datetime))]
  }
}

nexrad_key_to_url <- function(key) {
  paste0(
    getOption(
      "getRad.nexrad_data_url",
      default = "https://unidata-nexrad-level2.s3.amazonaws.com"
    ),
    "/",
    key
  )
}
