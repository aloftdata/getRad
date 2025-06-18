#' Get polar volume (PVOL) data from supported sources
#'
#' Gets polar volume data from supported sources and returns it as a (list of)
#' [polar volume objects][bioRad::summary.pvol]. The source is automatically
#' detected based on the provided `radar`.
#'
#' @details
#' For more details on supported sources, see `vignette("supported_sources")`.
#'
#' @param radar Name of the radar (odim code) as a character string (e.g.
#'   `"nlhrw"` or `"fikor"`).
#' @param datetime Either:
#'   - A single [`POSIXct`][base::DateTimeClasses], for which the most
#'   representative data file is downloaded. In most cases this will be the time
#'   before.
#'   - A [lubridate::interval()] or two [`POSIXct`][base::DateTimeClasses],
#'   between which all data files are downloaded.
#' @param ... Additional arguments passed on to reading functions, for example
#'   `param = "all"` to the [bioRad::read_pvolfile()].
#' @return Either a polar volume or a list of polar volumes. See
#'   [bioRad::summary.pvol()] for details.
#' @export
#' @examples
#' \dontrun{
#' # Get PVOL data for a single radar and datetime
#' get_pvol("deess", as.POSIXct(Sys.Date()))
#'
#' # Get PVOL data for multiple radars and a single datetime
#' get_pvol(
#'   c("deess", "dehnr", "fianj", "czska", "KABR"),
#'   as.POSIXct(Sys.Date())
#' )
#' }
get_pvol <- function(radar = NULL, datetime = NULL, ...) {
  check_odim_nexrad(radar)
  if (anyDuplicated(radar)) {
    cli::cli_abort(
      "The argument {.arg radar} contains duplications these should be removed.",
      class = "getRad_error_radar_duplicated"
    )
  }
  if (inherits(datetime, "POSIXct") && length(datetime) == 2) {
    if (any(duplicated(datetime))) {
      cli::cli_abort("When providing two {.cls POSIXct} as a {.arg datetime}
                     they should differ to represent an inverval.",
        class = "getRad_error_duplicated_timestamps"
      )
    }
    datetime <- lubridate::interval(min(datetime), max(datetime))
  }
  if (is.null(datetime) ||
    !inherits(datetime, c("POSIXct", "Interval")) ||
    !rlang::is_scalar_vector(datetime)
  ) {
    cli::cli_abort("The argument {.arg datetime} to the {.fn get_pvol} function
                   should be a single {.cls POSIXct} or a {.cls interval}.",
      class = "getRad_error_time_not_correct"
    )
  }


  safe_get_pvol <- purrr::possibly(get_pvol, otherwise = NULL, quiet = TRUE)

  # First start mapping over radars so later one only one radar is present. I
  # do however here already check if I can find a function for a radar to
  # ensure early failure and not first do a lot of download before failing on
  # the last radar
  if (length(radar) != 1) {
    purrr::map(radar, select_get_pvol_function) # quick check if all radars exist
    pvols <- purrr::map(radar, safe_get_pvol, datetime = datetime, ...)
    if (lubridate::is.interval(datetime)) {
      pvols <- unlist(pvols, recursive = F)
    }
    return(pvols)
  }


  fn <- select_get_pvol_function(radar)

  if (lubridate::is.interval(datetime)) {
    if (lubridate::as.duration(datetime) > lubridate::hours(1)) {
      cli::cli_warn("The interval specified for {.arg datetime} () likely results
                    in many polar volumes, when loading that may polar
                    volumes at the same time computational issues frequently
                    occur.",
        class = "getRad_warn_many_pvols_requested"
      )
    }
  }
  # for all but the us we can predict nominal times (every 5 minutes) and therefore we can do recursive calls to the respective function using one timestamp. In the US we call with an interval and in the function find the right keys
  if (fn != "get_pvol_us") {
    if (lubridate::is.interval(datetime)) {
      timerange <-
        lubridate::floor_date(
          seq(lubridate::int_start(datetime),
            lubridate::int_end(datetime) + lubridate::minutes(5),
            by = "5 mins"
          ),
          "5 mins"
        )
      datetime <- timerange[timerange %within% datetime]
      polar_volumes <- purrr::map(datetime, safe_get_pvol, radar = radar, ...)
      return(polar_volumes)
    } else {
      rlang::exec(fn, radar = radar, lubridate::floor_date(datetime, "5 mins"), ...)
    }
  } else {
    # For now then US data is request the interval if forwarded
    # get_pvol_us supports intervals
    rlang::exec(fn, radar = radar, datetime, ...)
  }
}


# Helper function to find the function for a specific radar
select_get_pvol_function <- function(radar, call=rlang::caller_env()) {
  if (is_nexrad(radar)) {
    return("get_pvol_us")
  }
  cntry_code <- substr(radar, 1, 2) # nolint
  fun <- (dplyr::case_when(
    cntry_code == "nl" ~ "get_pvol_nl",
    cntry_code == "fi" ~ "get_pvol_fi",
    cntry_code == "dk" ~ "get_pvol_dk",
    cntry_code == "de" ~ "get_pvol_de",
    cntry_code == "ee" ~ "get_pvol_ee",
    cntry_code == "cz" ~ "get_pvol_cz",
    .default = NA
  ))
  if (rlang::is_na(fun)) {
    cli::cli_abort(
      "No suitable function exist downloading from the radar {.val {radar}}",
      class = "getRad_error_no_function_for_radar_with_country_code",
      call=call
    )
  }
  return(fun)
}
