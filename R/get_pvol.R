#' Retrieve polar volumes
#'
#' @param radar The name of the radar (odim string) as a character string (e.g.
#'   `"nlhrw"` or `"fikor"`).
#' @param datetime The time as a `POSIXct` vector for the polar volume(s) to download or
#'    an [`interval`][lubridate::interval] in which all polar volumes need to be downloaded.
#' @param ... Additional arguments passed on to the individual reading functions, for example `param="all"` to the [bioRad::read_pvolfile()] function.
#'
#' @details
#'
#' For more details on specific countries please see the vignettes.
#'
#' @return Either a polar volume or a list of polar volumes
#' @export
#'
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
  if (is.null(radar) ||
    !rlang::is_character(radar) ||
    !all(nchar(radar) == 5) ||
    anyDuplicated(radar)) {
    cli::cli_abort("The argument {.arg radar} to the {.fn get_pvol} function should be a characters with each a length of 5 characters corresponding to ODIM codes. None should be duplicated.",
      class = "getRad_error_radar_not_character"
    )
  }
  if (is.null(datetime) ||
    !inherits(datetime, c("POSIXct","Interval")) ||
    anyDuplicated(datetime) ||
    (any((as.numeric(datetime) %% 300) != 0)&& inherits(datetime, "POSIXct"))) {
    cli::cli_abort("The argument {.arg datetime} to the {.fn get_pvol} function should be a POSIXct without duplications. All timestamps should be rounded to 5 minutes intervals.",
      class = "getRad_error_time_not_correct"
    )
  }
  if(lubridate::is.interval(datetime) && !rlang::is_scalar_vector(datetime)){
    cli::cli_abort("Only one `interval` can be provided as the {.arg datetime} argument.",
                   class="getRad_error_multiple_intervals_provided")
  }
  if(lubridate::is.interval(datetime)){
    timerange<-lubridate::floor_date(seq(lubridate::int_start(datetime), lubridate::int_end(datetime),"5 mins"),"5 mins")
    datetime<-timerange[timerange%within% datetime]
    if(length(datetime)>10){
      cli::cli_warn("The interval specified for {.arg datetime} resulted in {length(datetime)} timestamps, when loading that may polar volumes at the same time computational issues frequently occur.",class="getRad_warn_many_pvols_requested")
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
