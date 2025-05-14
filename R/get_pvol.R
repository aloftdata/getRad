#' Get polar volume (PVOL) data from supported countries
#'
#' Gets polar volume data for supported countries and returns it as a (list of)
#' [polar volume objects][bioRad::summary.pvol].
#'
#' @details
#' For more details on supported sources, see `vignette("supported_sources")`.
#'
#' @param radar Name of the radar (odim code) as a character string (e.g.
#'   `"nlhrw"` or `"fikor"`).
#' @param datetime Either:
#'   - A single [`POSIXct`][base::DateTimeClasses], for which the nearest data
#'   file is downloaded.
#'   - A [lubridate::interval()], between which all data files are downloaded.
#' @param ... Additional arguments passed on to reading functions, for example
#'   `param = "all"` to the [bioRad::read_pvolfile()].
#' @return Either a polar volume or a list of polar volumes. See
#'   [bioRad::summary.pvol()] for details.
#' @export
#' @examples
#' \dontrun{
#' get_pvol("deess", as.POSIXct(Sys.Date()))
#' get_pvol("czska", as.POSIXct(Sys.Date()))
#' get_pvol(
#'   c("deess", "dehnr", "fianj", "czska"),
#'   as.POSIXct(Sys.Date())
#' )
#' }
get_pvol <- function(radar = NULL, datetime = NULL, ...) {
  check_odim(radar)
  if(anyDuplicated(radar))
  {
    cli::cli_abort(
      "The argument {.arg radar} contains duplications these should be removed.",
      class="getRad_error_radar_duplicated"
    )
  }
  if (is.null(datetime) ||
      !inherits(datetime, c("POSIXct", "Interval")) ||
      anyDuplicated(datetime) ||
      (any((as.numeric(datetime) %% 300) != 0) && inherits(datetime, "POSIXct"))) {
    cli::cli_abort("The argument {.arg datetime} to the {.fn get_pvol} function
                   should be a POSIXct without duplications. All timestamps
                   should be rounded to 5 minutes intervals.",
      class = "getRad_error_time_not_correct"
    )
  }
  if (lubridate::is.interval(datetime) && !rlang::is_scalar_vector(datetime)) {
    cli::cli_abort(
      "Only one `interval` can be provided as the {.arg datetime} argument.",
      class = "getRad_error_multiple_intervals_provided"
    )
  }
  if (lubridate::is.interval(datetime)) {
    timerange <-
      lubridate::floor_date(
        seq(lubridate::int_start(datetime),
          lubridate::int_end(datetime),
          by = "5 mins"
        ),
        "5 mins"
      )
    datetime <- timerange[timerange %within% datetime]
    if (length(datetime) > 10) {
      cli::cli_warn("The interval specified for {.arg datetime} resulted in
                    {length(datetime)} timestamps, when loading that may polar
                    volumes at the same time computational issues frequently
                    occur.",
        class = "getRad_warn_many_pvols_requested"
      )
    }
  }

  if (length(datetime) != 1) {
    polar_volumes <- (purrr::map(datetime, get_pvol, radar = radar, ...))
    if (length(radar) != 1) {
      # in case multiple radars are requested the results of the recursive call
      # is a list of polar volumes, to prevent a nested list this unlist
      # statement is used
      polar_volumes <- unlist(polar_volumes, recursive = FALSE)
    }
    return(polar_volumes)
  }
  if (length(radar) != 1) {
    return(purrr::map(radar, get_pvol, datetime = datetime, ...))
  }

  fn <- select_get_pvol_function(radar)
  get(fn)(radar, datetime, ...)
}


# Helper function to find the function for a specific radar
select_get_pvol_function <- function(radar) {
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
      "No suitable function exist downloading from the radar {radar}",
      class = "getRad_error_no_function_for_radar_with_country_code"
    )
  }
  return(fun)
}
