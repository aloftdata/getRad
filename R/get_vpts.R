#' Get vertical profile time series (VPTS) data from supported sources
#'
#' Gets vertical profile time series data from supported sources and returns it
#' as a (list of) of [vpts objects][bioRad::summary.vpts] or a
#' [dplyr::tibble()].
#'
#' @inheritParams get_pvol
#' @inherit get_vpts_aloft details
#' @param datetime Either:
#'   - A [`POSIXct`][base::DateTimeClasses] datetime (or `character`
#'   representation), for which the data file is downloaded.
#'   - A [`Date`][base::Dates] date (or `character` representation), for which
#'   all data files are downloaded.
#'   - A vector of datetimes or dates, between which all data files are
#'   downloaded.
#'   - A [lubridate::interval()], between which all data files are downloaded.
#' @param source Source of the data. One of `"baltrad"`, `"uva"`, `"ecog-04003"`
#'   or `"rmi"`. Only one source can be queried at a time. If no source is
#'   provided `baltrad` is used.
#' @param return_type Type of object that should be returned. Either:
#'   - `"vpts"`: vpts object(s) (default).
#'   - `"tibble"`: a [dplyr::tibble()].
#' @return Either a vpts object, a list of vpts objects or a tibble. See
#'   [bioRad::summary.vpts] for details.
#' @export
#' @examplesIf interactive()
#' # Get VPTS data for a single radar and date
#' get_vpts(radar = "bejab", datetime = "2023-01-01", source = "baltrad")
#' get_vpts(radar = "bejab", datetime = "2020-01-19", source = "rmi")
#'
#' # Get VPTS data for multiple radars and a single date
#' get_vpts(
#'   radar = c("dehnr", "deflg"),
#'   datetime = lubridate::ymd("20171015"),
#'   source = "baltrad"
#' )
#'
#' # Get VPTS data for a single radar and a date range
#' get_vpts(
#'   radar = "bejab",
#'   datetime = lubridate::interval(
#'     lubridate::ymd_hms("2023-01-01 00:00:00"),
#'     lubridate::ymd_hms("2023-01-02 00:14:00")
#'   ),
#'   source = "baltrad"
#' )
#' get_vpts("bejab", lubridate::interval("20210101", "20210301"))
#'
#' # Get VPTS data for a single radar, date range and non-default source
#' get_vpts(radar = "bejab", datetime = "2016-09-29", source = "ecog-04003")
#'
#' # Return a tibble instead of a vpts object
#' get_vpts(
#'   radar = "chlem",
#'   date = "2023-03-10",
#'   source = "baltrad",
#'   return_type = "tibble"
#' )
get_vpts <- function(radar,
                     datetime,
                     source = c("baltrad", "uva", "ecog-04003", "rmi"),
                     return_type = c("vpts", "tibble")) {
  # Check source argument
  ## If no source is provided, set "baltrad" as default
  if (missing(source)) {
    source <- "baltrad"
  }
  if (is.null(source)) {
    # providing NULL isn't allowed either
    cli::cli_abort(
      glue::glue(
        "Please provide a value for the source argument:
        possible values are {possible_sources}.",
        possible_sources = glue::glue_collapse(glue::backtick(source),
          sep = ", ",
          last = " or "
        )
      ),
      class = "getRad_error_source_missing"
    )
  }

  ## Only a single source can be fetched from at a time, and it must be one of
  ## the provided values in the enumeration. New sources must also be added to
  ## the enumeration in the function definition.
  if (length(source) > 1) {
    cli::cli_abort(
      "Only one source can be queried at a time.",
      class = "getRad_error_multiple_sources"
    )
  }

  ## The provided source must be one of the supported values in the enumeration

  # Get the default value of the source arg, even if the user provided
  # a different value.
  supported_sources <- eval(formals()$source)
  if (!source %in% supported_sources) {
    cli::cli_abort(
      glue::glue(
        "Invalid source {glue::backtick(source)} provided. Possible values are:
        {possible_sources}.",
        possible_sources = glue::glue_collapse(
          glue::backtick(supported_sources),
          sep = ", ",
          last = " or "
        )
      ),
      class = "getRad_error_source_invalid"
    )
  }

  # Rename radar & source arguments so it's clear that it can contain multiple
  # radars
  selected_radars <- radar
  selected_source <- source

  # Check that the provided radar argument is a character vector
  if (!is.character(selected_radars)) {
    cli::cli_abort(
      "Radar argument must be a character vector.",
      class = "getRad_error_radar_not_character"
    )
  }

  # Check that the provided date argument is parsable as a date or interval
  if (!is.character(datetime) &&
    !lubridate::is.timepoint(datetime) &&
    !lubridate::is.interval(datetime)) {
    cli::cli_abort(
      "{.arg datetime} argument must be a character, POSIXct, Date, or Interval object.",
      class = "getRad_error_date_parsable"
    )
  }
  # Parse the provided date argument to a lubridate interval
  ## If the date is a single date, convert it to an interval by adding a whole
  ## day, minus a second
  if (!inherits(datetime, "Interval")) {
    datetime_converted <- lubridate::as_datetime(datetime)
    ### If time information is provided
    if (any(datetime_converted != lubridate::as_datetime(lubridate::as_date(datetime_converted))) ||
      inherits(datetime, "POSIXct")) {
      # timestamp like `datetime`
      date_interval <-
        lubridate::interval(
          ### starting at the datetime itself
          min(datetime_converted),
          ### to the end of the day
          max(datetime_converted)
        )
      ### If only date information is provided
    } else {
      # date like `datetime`
      date_interval <-
        lubridate::interval(
          ### starting at the datetime itself
          min(datetime_converted),
          ### to the end of the day
          end_of_day(max(datetime_converted))
        )
    }
  } else {
    date_interval <- datetime
  }

  ## We need to round the interval because coverage only has daily resolution
  rounded_interval <- round_interval(date_interval, "day")

  # Query the selected radars and fetched coverage for aloft vpts data.
  # fetched_vpts <-
  #   purrr::map(
  #     selected_radars,
  #     ~ get_vpts_aloft(
  #       .x,
  #       rounded_interval = rounded_interval,
  #       source = selected_source,
  #       coverage = coverage
  #     )
  #   ) |>
  #   radar_to_name()

  fetched_vpts <-
    switch(source,
           c(rmi = get_vpts_rmi(radar, rounded_interval),
             purrr::set_names(
               purrr::map(selected_radars,
                          ~ get_vpts_aloft(.x,
                                           rounded_interval = rounded_interval,
                                           source = selected_source,
                                           coverage = coverage)),
               nm = c("uva", "ecog-04003", "rmi")
             )))

  # Drop any results outside the requested interval
  filtered_vpts <-
    fetched_vpts |>
    purrr::map(
      \(df) {
        dplyr::mutate(df,
          datetime = lubridate::as_datetime(.data$datetime)
        )
      }
    ) |>
    purrr::map(
      \(df) {
        dplyr::filter(
          df,
          .data$datetime %within% date_interval
        )
      }
    )

  # Return the vpts data
  ## By default, return drop the source column and convert to a vpts object for
  ## usage in bioRAD
  return_type <- rlang::arg_match(return_type)
  ## Depending on the value of the `return_type` argument, do some final
  ## formatting or conversion
  return_object <-
  switch(return_type,
    tibble = purrr::list_rbind(filtered_vpts),
    vpts = (\(filtered_vpts) {
      filtered_vpts_no_source <-
        purrr::map(filtered_vpts, \(df) dplyr::select(df, -source))
      vpts_list <- purrr::map(filtered_vpts_no_source, bioRad::as.vpts)
      # If we are only returning a single radar, don't return a list
      if (length(vpts_list) == 1) {
        return(purrr::chuck(vpts_list, 1))
      } else {
        return(vpts_list)
      }
    })(filtered_vpts)
  )
  # Return the converted/formatted object
  return(return_object)

}
