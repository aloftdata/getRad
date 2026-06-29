#' Get vertical profile time series (VPTS) data from supported sources
#'
#' Gets vertical profile time series data from supported sources and returns it
#' as a (list of) of [vpts objects][bioRad::summary.vpts] or a
#' [dplyr::tibble()].
#'
#' @details
#' For more details on supported sources, see `vignette("supported_sources")`.
#'
#'   In case data is read from a directory, file in the directory
#'   should be structures like they are in the monthly folders of the aloft
#'   repository. To specify an alternative structure the
#'   `"getRad.vpts_local_path_format_aloft"` option can be used. This can, for
#'   example, be used to read daily data. Some example options for the glue
#'   formatters are:
#'
#'  * `"{radar}/{year}/{radar}_vpts_{year}{month}.csv.gz"`: The default format,
#'  the same structure as the monthly directories in the aloft repository. Or as
#'  contained in the `tgz` files in the aloft zenodo repository.
#'  *  `"{substr(radar, 1,2)}/{radar}/{year}/{radar}_vpts_{year}{month}.csv.gz"`:
#'  The format as in the files in the zenodo aloft repository
#'  * `"{radar}/{year}/{radar}_vpts_{year}{month}{day}.csv"`: The format as daily
#'  data is stored in aloft data
#'
#'  A similar option (`"getRad.vpts_local_path_format_aloft"`) exist for reading
#'  dark ecology data. The default value here is `"getRad.vpts_local_path_format_aloft"`.
#'  Here the option does refer to the directories where the dark ecology files
#'  should be searched.
#'
#'  Besides the examples above there is a `date` object available for formatting. Note
#'  that `day` and `month` are zero padded character strings in the glue formating.
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
#' @param source Source of the data. One of `"baltrad"`, `"uva"`,
#'   `"ecog-04003"`, `"rmi"`, `"dark_ecology"` or `"birdcast"`. Only one source
#'   can be queried at a time. If not provided, `"baltrad"` is used.
#' @param return_type Type of object that should be returned. Either:
#'   - `"vpts"`: vpts object(s) (default).
#'   - `"tibble"`: a [dplyr::tibble()].
#' @param ... Optional arguments, to [bioRad::read_cajun()] when reading
#'   `"dark_ecology"` data.
#' @param path A local directory where data are read from. If specified the file
#'   structure is taken from the `source` argument. See details for an
#'   explanation of the file format.
#' @returns Either a vpts object, a list of vpts objects or a tibble. See
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
#'   datetime = "2023-03-10",
#'   source = "baltrad",
#'   return_type = "tibble"
#' )
#' #' Get VPTS data from the public BirdCast NEXRAD archive
#' get_vpts(radar = "KABR", datetime = "2023-01-01", source = "birdcast")
get_vpts <- function(
  radar,
  datetime,
  source = c("baltrad", "uva", "ecog-04003", "rmi", "birdcast", "dark_ecology"),
  return_type = c("vpts", "tibble"),
  ...,
  path = NULL
) {
  # Input checks ----
  # Check source argument
  ## If no source is provided, set "baltrad" as default
  if (missing(source)) {
    source <- "baltrad"
  }
  supported_sources <- eval(rlang::fn_fmls()$source)

  if (is.null(source)) {
    # providing NULL isn't allowed either
    cli::cli_abort(
      c(
        "{.arg source} must be provided.",
        "i" = "Supported sources: {.val {supported_sources}}."
      ),
      class = "getRad_error_source_missing"
    )
  }

  ## Only a single source can be fetched from at a time, and it must be one of
  ## the provided values in the enumeration. New sources must also be added to
  ## the enumeration in the function definition.
  if (length(source) > 1) {
    cli::cli_abort(
      "{.arg source} must be a single character value.",
      class = "getRad_error_multiple_sources"
    )
  }

  ## The provided source must be one of the supported values in the enumeration

  # Get the default value of the source arg, even if the user provided
  # a different value.
  supported_sources <- eval(formals()$source)
  if (!(source %in% supported_sources)) {
    cli::cli_abort(
      c(
        "{.arg source} {.val {source}} is invalid.",
        "i" = "Supported sources: {.val {supported_sources}}."
      ),
      class = "getRad_error_source_invalid"
    )
  }

  # Check that the provided radar argument is a character vector
  if (!is.character(radar)) {
    cli::cli_abort(
      "{.arg radar} must be a character vector.",
      class = "getRad_error_radar_not_character"
    )
  }

  # Check that the provided date argument is parsable as a date or interval
  if (
    !is.character(datetime) &&
      !lubridate::is.timepoint(datetime) &&
      !lubridate::is.interval(datetime)
  ) {
    cli::cli_abort(
      "{.arg datetime} must be a {.cls character}, {.cls POSIXct}, {.cls Date},
       or {.cls Interval} object.",
      class = "getRad_error_date_parsable"
    )
  }
  # Parse the provided date argument to a lubridate interval ----
  ## If the date is a single date, convert it to an interval
  if (!inherits(datetime, "Interval")) {
    datetime_converted <- lubridate::as_datetime(datetime)
    ### If time information is provided
    if (
      any(
        datetime_converted !=
          lubridate::as_datetime(lubridate::as_date(datetime_converted))
      ) ||
        inherits(datetime, "POSIXct")
    ) {
      # timestamp like `datetime`
      if (length(datetime) == 1) {
        # if only one timestamps is provided generate the 5 minute floored interval
        date_interval <-
          lubridate::interval(
            ### starting at the nominal date time
            lubridate::floor_date(datetime_converted, "5 mins"),
            ### to the end of the 5 minutes interval
            lubridate::floor_date(datetime_converted, "5 mins") +
              lubridate::minutes(5) -
              lubridate::milliseconds(1)
          )
      } else {
        date_interval <-
          lubridate::interval(
            ### starting at the datetime itself
            min(datetime_converted),
            ### to the end of the day
            max(datetime_converted)
          )
      }
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

  ## Round the interval because the helpers always fetch data a day at a time ----
  date_interval_utc <- lubridate::as.interval(
    lubridate::with_tz(lubridate::int_start(date_interval), "UTC"),
    lubridate::with_tz(lubridate::int_end(date_interval), "UTC")
  )
  rounded_interval <- round_interval(date_interval_utc, "day")

  # Query the selected radars ----
  # Directing to the correct get_vpts_* helper based on source.
  cl <- rlang::caller_env(0)

  ## Split of local path (here we know there is a single valid source argument)
  if (!missing(path)) {
    fetched_vpts <- get_vpts_local(
      radar = radar,
      rounded_interval = rounded_interval,
      source = source,
      path = path,
      ...
    )
  } else {
    aloft_sources <- eval(formals("get_vpts_aloft")$source)

    source_type <- dplyr::case_when(
      source == "rmi" ~ "rmi",
      source == "birdcast" ~ "birdcast",
      source %in% aloft_sources ~ "aloft"
    )

    fetched_vpts <-
      switch(
        source_type,
        rmi = purrr::map(
          radar,
          ~ get_vpts_rmi(.x, rounded_interval),
          .purrr_error_call = cl
        ),
        aloft = purrr::map(
          radar,
          ~ get_vpts_aloft(
            .x,
            rounded_interval = rounded_interval,
            source = source
          ),
          .purrr_error_call = cl
        ),
        birdcast = purrr::map(
          radar,
          ~ get_vpts_birdcast(
            .x,
            rounded_interval = rounded_interval
          ),
          .purrr_error_call = cl
        )
      ) |>
      radar_to_name()
  }
  # Return the vpts data ----
  ## By default, return drop the source column and convert to a vpts object for
  ## usage in bioRad
  return_type <- rlang::arg_match(return_type)

  # dark ecology local now only returns vpts
  if (any(purrr::map_lgl(fetched_vpts, inherits, "vpts"))) {
    if (return_type != "vpts") {
      cli::cli_abort(
        "For the {.arg source} {.val {source}} the {.arg return_type} {.val {return_type}} is
                           currently not supported. Only a {.cls vpts} can be returned.",
        class = "getRad_error_vpts_not_supported_return_type"
      )
    }
    fetched_vpts <- purrr::map(
      fetched_vpts,
      ~ .x[lubridate::`%within%`(.x$datetime, date_interval)]
    ) |>
      purrr::map(add_reference_vpts, source = source)
    if (length(fetched_vpts) != 1) {
      return(fetched_vpts)
    } else {
      return(purrr::chuck(fetched_vpts, 1))
    }
  }

  # Drop any results outside the requested interval ----
  filtered_vpts <-
    fetched_vpts |>
    purrr::map(
      \(df) {
        dplyr::mutate(df, datetime = lubridate::as_datetime(.data$datetime))
      },
      .purrr_error_call = cl
    ) |>
    purrr::map(
      \(df) {
        dplyr::filter(
          df,
          .data$datetime %within% date_interval
        )
      },
      .purrr_error_call = cl
    )
  ## Depending on the value of the `return_type` argument, do some final
  ## formatting or conversion
  return_object <-
    switch(
      return_type,
      tibble = purrr::list_rbind(filtered_vpts),
      vpts = (\(filtered_vpts) {
        filtered_vpts_no_source <-
          purrr::map(
            filtered_vpts,
            \(df) dplyr::select(df, -source),
            .purrr_error_call = cl
          )
        vpts_list <- purrr::map(
          filtered_vpts_no_source,
          bioRad::as.vpts
        ) |>
          purrr::map(add_reference_vpts, source = source)
        # If we are only returning a single radar, don't return a list
        if (length(vpts_list) == 1) {
          return(purrr::chuck(vpts_list, 1))
        } else {
          return(vpts_list)
        }
      })(filtered_vpts)
    )
  # Return the converted/formatted object ----
  return(return_object)
}
