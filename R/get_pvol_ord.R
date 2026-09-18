#' @importFrom utils modifyList
NULL

#' Merge a list of attribute table into one, warn for conflicting attributes
#'
#' @param attribute_list A list of attributes tables from a bioRad object to merge
#' @param type The time of object, used for clarifying the warning
#'
#'
#'
#' @noRd
#' @examples
#'
#'
#' merge_attributes(list(   list(how = list(NI = 24),
#'        what = list(
#'       startdate = "20151018", starttime = "180003"
#'     ), where = list(
#'       elangle = 0.5, nbins = 480L, nrays = 360L
#'     )))
#'   ,
#'   "scan"
#' )
#' # Two identical
#' merge_attributes(list(   list(how = list(NI = 24),
#'                               what = list(
#'                                 startdate = "20151018", starttime = "180003"
#'                               ), where = list(
#'                                 elangle = 0.5, nbins = 480L, nrays = 360L
#'                               )),list(how = list(NI = 24),
#'                                       what = list(
#'                                         startdate = "20151018", starttime = "180003"
#'                                       ), where = list(
#'                                         elangle = 0.5, nbins = 480L, nrays = 360L
#'                                       )))
#'                  ,
#'                  "scan"
#')
#'
#' # Two both with missing data
#' merge_attributes(list(   list(how = list(NI = 24),
#'                               what = list(
#'                                 startdate = "20151018"
#'                               ), where = list(
#'                                 elangle = 0.5, nbins = 480L, nrays = 360L
#'                               )),list(how = list(NI = 24),
#'                                       what = list(
#'                                         startdate = "20151018", starttime = "180003"
#'                                       ), where = list(
#'                                         elangle = 0.5,  nrays = 360L
#'                                       )))
#'                  ,
#'                  "scan"
#' )
#'
#' # Two both with missing and conflicting data
#' merge_attributes(list(   list(how = list(NI = 23),
#'                               what = list(
#'                                 startdate = "20151018"
#'                               ), where = list(
#'                                 elangle = 0.5, nbins = 480L, nrays = 360L
#'                               )),list(how = list(NI = 24),
#'                                       what = list(
#'                                         startdate = "20151018", starttime = "180003"
#'                                       ), where = list(
#'                                         elangle = 0.5,  nrays = 360L
#'                                       )))
#'                  ,
#'                  "scan"
#')
merge_attributes <- function(attribute_list, type) {
  attributes_new <- purrr::reduce(attribute_list, modifyList)
  if (
    !all(purrr::map_lgl(
      attribute_list,
      ~ identical(attributes_new, modifyList(attributes_new, .x, keep.null = T))
    ))
  ) {
    removed <- c()
    for (i in names(attributes_new)) {
      for (j in names(attributes_new[[i]])) {
        remove <- F
        for (a in attribute_list) {
          if (
            !is.null(a[[i]][[j]]) &&
              !identical(a[[i]][[j]], attributes_new[[i]][[j]])
          ) {
            remove <- T
          }
        }
        if (remove) {
          attributes_new[[i]][[j]] <- NULL
          removed <- c(removed, paste(i, j, sep = "/"))
        }
      }
    }
    cli::cli_warn(
      "The following {type} attributes are removed as they differ within the scan {.val {removed}}.",
      class = "getRad_warn_ord_conflicting_attributes"
    )
  }
  return(attributes_new)
}

#' Merge a list of scans into one scan
#'
#' Frequently different parameters are stored in ths separate scans this function can help merging them
#'
#' @param scan_list A list of scans
#' @param ... not used
#' @param call the calling environment for error messages
#'
#' @returns a bioRad::scan
#' @noRd
#'
#' @examples
#'
#' merge_scans(list(bioRad::example_scan |> dplyr::select("DBZH"), bioRad::example_scan |> dplyr::select(-DBZH)))
merge_scans <- function(scan_list, ..., call = rlang::caller_env()) {
  geos <- purrr::map(scan_list, purrr::pluck, 'geo')
  rscales <- purrr::map_vec(geos, purrr::pluck, 'rscale')
  if (dplyr::n_distinct(rscales) != 1) {
    cli::cli_warn(
      "Data contains multiple range scales ({.val {rscales}}), therefore they are omitted in the scan {.attr geos} attribute.",
      class = "getRad_warn_ord_scan_conflicting_rscales"
    )
    geos <- purrr::modify(geos, purrr::list_modify, rscale = rlang::zap())
  }
  if (dplyr::n_distinct(geos) != 1) {
    cli::cli_abort(
      "Trying to merge scan data from multiple locations",
      class = "getRad_error_ord_scan_multi_geo",
      call = call
    )
  }
  geo <- unique(geos)[[1]]

  radars <- purrr::map_chr(scan_list, purrr::pluck, 'radar')
  if (dplyr::n_distinct(radars) != 1) {
    cli::cli_abort(
      "Trying to merge scan data from multiple radars",
      class = "getRad_error_ord_scan_multi_radar",
      call = call
    )
  }
  radar <- unique(radars)
  datetimes <- purrr::map_vec(scan_list, purrr::pluck, 'datetime')
  if (
    is_odim_country(radar, "no") &&
      dplyr::n_distinct(datetimes) == 2
  ) {
    datetime <- min(datetimes)
    # Norway has pvols starting at slightly different times every 10 minutes
  } else {
    datetime <- unique(datetimes)
  }
  if (dplyr::n_distinct(datetime) != 1) {
    cli::cli_abort(
      "Trying to merge data from multiple timestamps",
      class = "getRad_error_ord_scan_multi_time",
      call = call
    )
  }
  attributes <- merge_attributes(
    purrr::map(
      scan_list,
      purrr::pluck,
      'attributes'
    ),
    type = "scan"
  )

  names(scan_list) <- NULL
  params <- purrr::map(scan_list, purrr::pluck, 'params') |>
    unlist(recursive = F)
  params_attr <- purrr::map_chr(params, attr, 'param')
  if (any(params_attr != names(params)) | anyDuplicated(params_attr)) {
    cli::cli_abort(
      "params cant be merged",
      class = "getRad_error_ord_scan_duplicated_param",
      call = call
    )
  }
  new_scan <- structure(
    list(
      radar = radar,
      datetime = datetime,
      params = params,
      attributes = attributes,
      geo = geo
    ),
    class = "scan"
  )
  if (!bioRad::is.scan(new_scan)) {
    cli::cli_abort(
      "no valid scan constructed",
      class = "getRad_error_ord_scan_invalid",
      call = call
    )
  }
  return(new_scan)
}
#' Merge polar volumes containing different scans
#'
#' @param pvol_list a list of polar volumes
#' @inheritParams merge_scan ... call
#'
#' @returns a single polar volume
#' @noRd
merge_pvols <- function(pvol_list, ..., call = rlang::caller_env()) {
  radars <- purrr::map_chr(pvol_list, purrr::pluck, 'radar')
  if (dplyr::n_distinct(radars) != 1) {
    cli::cli_abort(
      "Trying to merge data from multiple radars",
      class = "getRad_error_ord_pvol_multi_radar",
      call = call
    )
  }
  radar <- unique(radars)
  datetimes <- purrr::map_vec(pvol_list, purrr::pluck, 'datetime')
  if (dplyr::n_distinct(lubridate::floor_date(datetimes, '5 mins')) != 1) {
    # Norway has pvols starting at slightly different times
    cli::cli_abort(
      "Trying to merge data from multiple timestamps",
      class = "getRad_error_ord_pvol_multi_time",
      call = call
    )
  }
  datetime <- min(datetimes)

  geos <- purrr::map(pvol_list, purrr::pluck, 'geo')
  if (dplyr::n_distinct(geos) != 1) {
    cli::cli_abort(
      "Trying to merge data from multiple locations",
      class = "getRad_error_ord_pvol_multi_geo",
      call = call
    )
  }
  geo <- unique(geos)[[1]]
  attributes <- merge_attributes(
    purrr::map(
      pvol_list,
      purrr::pluck,
      'attributes'
    ),
    type = "pvol"
  )
  scans <- purrr::map(pvol_list, purrr::pluck, 'scans') |> unlist(recursive = F)
  split_df <- purrr::map(
    scans,
    ~ bioRad::attribute_table(
      .x,
      select = c("what.startdate", "what.starttime", "how.scan_index")
    ) |>
      dplyr::select(!dplyr::matches("param"))
  ) |>
    dplyr::bind_rows()
  split_df$where.elangle <- purrr::map_dbl(
    scans,
    ~ bioRad::get_elevation_angles(.x)
  )

  split(
    scans,
    split_df,
    drop = T
  ) |>
    purrr::set_names(NULL) |>
    purrr::map(merge_scans, call = call) -> new_scans
  new_pvol <- structure(
    list(
      radar = radar,
      datetime = datetime,
      scans = new_scans,
      attributes = attributes,
      geo = geo
    ),
    class = "pvol"
  )
  if (!bioRad::is.pvol(new_pvol)) {
    cli::cli_abort(
      "No valid pvol constructed",
      class = "getRad_error_ord_pvol_invalid",
      call = call
    )
  }
  return(new_pvol)
}

get_pvol_ord <- function(radar, time, ..., call = rlang::caller_env()) {
  # url <- "https://s3.waw3-1.cloudferro.com/openradar-24h/"
  if (is_odim_country(radar, "pl")) {
    cli::cli_warn(
      c(
        "Polish radar data has conflicting scan attributes. As a result it is unclear if and how these data should be merged."
      ),
      class = "getRad_warn_ord_polish_scans"
    )
  }
  if (
    is_odim_country(radar, "ie") &&
      lubridate::floor_date(time, "15 min") ==
        lubridate::floor_date(time, "5 min")
  ) {
    cli::cli_abort(
      c(
        x = "No merging strategy for Irish radars has been implemented for times in the full quarter.",
        i = "Other times are ok and can be loaded."
      ),
      call = call,
      class = "getRad_warn_ord_irish_merging"
    )
  }

  base_url <- getOption(
    "getRad.opera_ord_base_url",
    default = "s3.waw3-1.cloudferro.com"
  )
  rlang::check_installed(
    "aws.s3",
    reason = "to download data from the Opera open radar data."
  )
  # list all keys for a radar and time combination
  keys <- aws.s3::get_bucket_df(
    "openradar-24h",
    prefix = glue::glue(
      '{format(time,"%Y/%m/%d", tz="UTC")}/{toupper(substr(1,2,x=radar))}/{tolower(radar)}'
    ),
    base_url = base_url,
    region = "",
    max = Inf
  )
  # Extract time from the keys and floor them to assign them to a pvol
  t_floored <- purrr::map_chr(strsplit(keys$Key, '@'), purrr::pluck, 2) |>
    lubridate::parse_date_time(orders = '%Y%m%dT%H%M', tz = 'UTC', exact = T) |>
    lubridate::floor_date("5 mins")

  keys_selected <- keys$Key[
    t_floored %in%
      lubridate::floor_date(lubridate::with_tz(time, 'UTC'), "5 mins")
  ]
  if (length(keys_selected) < 1) {
    cli::cli_abort(
      c(
        x = "No data was found for the selected combination of radar and time.",
        i = "Make sure a valid odim code is used to identify the radar and a timestamp within the last 24 hours."
      ),
      class = "getRad_error_ord_no_keys_found",
      call = call
    )
  }
  # Figureout if the odim files are SCANS or PVOLS and read accordingly
  type <- unique(purrr::map_chr(strsplit(keys_selected, '/'), purrr::pluck, 6))
  if (dplyr::n_distinct(type) != 1 || !(type %in% c("SCAN", "PVOL"))) {
    cli::cli_abort(
      "Mixed types should not occur.",
      class = "getRad_error_ord_mixed_types",
      call = call
    )
  }
  if (type == "PVOL") {
    pvols <- purrr::map(
      keys_selected,
      ~ aws.s3::s3read_using(
        object = .x,
        bioRad::read_pvolfile,
        bucket = "openradar-24h",
        opts = list(
          base_url = base_url,
          region = ""
        )
      )
    )
    pvol <- merge_pvols(pvols, call = call)
  } else {
    scans <- purrr::map(
      keys_selected,
      ~ aws.s3::s3read_using(
        object = .x,
        read_scan,
        bucket = "openradar-24h",
        opts = list(
          base_url = base_url,
          region = ""
        )
      )
    )
    scans <- purrr::map(
      scans,
      bioRad::attribute_table,
      select = c("what.startdate", "what.starttime", "how.scan_index")
    ) |>
      purrr::list_rbind() |>
      dplyr::select(!dplyr::matches("param")) |>
      tibble::add_column(
        where.elangle = purrr::map_dbl(scans, bioRad::get_elevation_angles)
      ) |>
      split(x = scans, drop = TRUE) |>
      rlang::set_names(NULL) |>
      purrr::map(merge_scans)

    pvol <- list_to_pvol(
      scans,
      time = lubridate::with_tz(lubridate::floor_date(time, '5 mins'), "UTC"),
      radar = radar,
      source = glue::glue("NOD:{radar},CMT:constructed from opera ord")
    )
  }
  return(pvol)
}
