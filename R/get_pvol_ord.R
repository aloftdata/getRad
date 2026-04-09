merge_attributes <- function(attribute_list) {
  attributes_new <- purrr::reduce(attribute_list, modifyList)
  if (
    !all(purrr::map_lgl(
      attribute_list,
      ~ identical(attributes_new, modifyList(attributes_new, .x))
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
      "The following attributes are removed as they differ within the scan {removed}"
    )
  }
  return(attributes_new)
}

merge_scans <- function(scan_list) {
  geos <- purrr::map(scan_list, purrr::pluck, 'geo')

  rscales <- purrr::map_vec(geos, purrr::pluck, 'rscale')
  if (dplyr::n_distinct(rscales) != 1) {
    ## Possibly warn
    geos <- purrr::modify(geos, purrr::list_modify, rscale = rlang::zap())
  }
  if (dplyr::n_distinct(geos) != 1) {
    cli::cli_abort("Trying to merge scan data from multiple locations")
  }
  geo <- unique(geos)[[1]]

  radars <- purrr::map_chr(scan_list, purrr::pluck, 'radar')
  if (dplyr::n_distinct(radars) != 1) {
    cli::cli_abort("Trying to merge scan data from multiple radars")
  }
  radar <- unique(radars)
  datetimes <- purrr::map_vec(scan_list, purrr::pluck, 'datetime')
  if (dplyr::n_distinct(lubridate::floor_date(datetimes, '5 mins')) != 1) {
    # Norway has pvols starting at slightly different times
    cli::cli_abort("Trying to merge data from multiple timestamps")
  }
  datetime <- min(datetimes)
  attributes <- merge_attributes(purrr::map(
    scan_list,
    purrr::pluck,
    'attributes'
  ))

  names(scan_list) <- NULL
  params <- purrr::map(scan_list, purrr::pluck, 'params') |>
    unlist(recursive = F)
  params_attr <- purrr::map_chr(params, attr, 'param')
  if (any(params_attr != names(params)) | anyDuplicated(params_attr)) {
    cli::cli_abort("params cant be merged")
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
    cli::cli_abort("no valid scan constructed")
  }
  return(new_scan)
}
merge_pvols <- function(pvol_list) {
  radars <- purrr::map_chr(pvol_list, purrr::pluck, 'radar')
  if (dplyr::n_distinct(radars) != 1) {
    browser()
    cli::cli_abort("Trying to merge data from multiple radars")
  }
  radar <- unique(radars)
  datetimes <- purrr::map_vec(pvol_list, purrr::pluck, 'datetime')
  if (dplyr::n_distinct(lubridate::floor_date(datetimes, '5 mins')) != 1) {
    # Norway has pvols starting at slightly different times
    cli::cli_abort("Trying to merge data from multiple timestamps")
  }
  datetime <- min(datetimes)

  geos <- purrr::map(pvol_list, purrr::pluck, 'geo')
  if (dplyr::n_distinct(geos) != 1) {
    cli::cli_abort("Trying to merge data from multiple locations")
  }
  geo <- unique(geos)[[1]]
  attributes <- merge_attributes(purrr::map(
    pvol_list,
    purrr::pluck,
    'attributes'
  ))
  # first_time <- sort(purrr::map_chr(attributes, ~ .x$what$time))[1]
  # attributes <- purrr::map(attributes, \(x) {
  #   x$what$time <- first_time
  #   x
  # })
  # endepochs <- (purrr::map_vec(attributes, ~ .x$how$endepoch))
  # if (dplyr::n_distinct(endepochs) != 1) {
  #   # possibly warn here
  #   attributes <- purrr::map(attributes, \(x) {
  #     x$how$endepochs <- endepochs
  #     x
  #   })
  # }
  # tasks <- (purrr::map_vec(attributes, ~ .x$how$task))
  # if (dplyr::n_distinct(tasks) != 1) {
  #   # possibly warn here
  #   # BEWID
  #   attributes <- purrr::map(attributes, \(x) {
  #     x$how$task <- tasks
  #     x
  #   })
  # }
  # if (dplyr::n_distinct(attributes) != 1) {
  #   browser()
  #   cli::cli_abort("Trying to merge data from multiple attributes")
  # }
  # attributes <- unique(attributes)[[1]]
  scans <- purrr::map(pvol_list, purrr::pluck, 'scans') |> unlist(recursive = F)
  split_df <- purrr::map(
    scans,
    ~ bioRad::attribute_table(
      .x,
      select = c("what.startdate", "what.starttime", "how.scan_index")
    ) |>
      dplyr::select(-param)
  ) |>
    dplyr::bind_rows()
  split_df$where.elangle <- purrr::map_dbl(
    scans,
    ~ bioRad::get_elevation_angles(.x)
  )
  scans_list <- split(
    scans,
    split_df,
    drop = T
  )
  names(scans_list) <- NULL
  new_scans <- purrr::map(scans_list, merge_scans)
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
    cli::cli_abort("no valid pvol constructed")
  }
  return(new_pvol)
}

get_pvol_ord <- function(radar, time, ..., call = rlang::caller_env()) {
  # time <- Sys.time()
  # radar <- "nobml"
  # url <- "https://s3.waw3-1.cloudferro.com/openradar-24h/"

  keys <- aws.s3::get_bucket_df(
    "openradar-24h",
    prefix = glue::glue(
      '{format(time,"%Y/%m/%d", tz="UTC")}/{toupper(substr(1,2,x=radar))}/{tolower(radar)}'
    ),
    base_url = "s3.waw3-1.cloudferro.com",
    region = "",
    max = Inf
  )
  t_floored <- purrr::map_chr(strsplit(keys$Key, '@'), purrr::pluck, 2) |>
    lubridate::parse_date_time(orders = '%Y%m%dT%H%M', tz = 'UTC', exact = T) |>
    lubridate::floor_date("5 mins")

  keys_selected <- keys$Key[
    t_floored %in%
      lubridate::floor_date(lubridate::with_tz(time, 'UTC'), "5 mins")
  ]
  if (length(keys_selected) < 1) {
    browser()
  }

  type <- unique(purrr::map_chr(strsplit(keys_selected, '/'), purrr::pluck, 6))
  if (dplyr::n_distinct(type) != 1 | !(type %in% c("SCAN", "PVOL"))) {
    cli::cli_abort("mixed types should not occur")
  }
  if (type == "PVOL") {
    pvols <- purrr::map(
      keys_selected,
      ~ aws.s3::s3read_using(
        object = .x,
        bioRad::read_pvolfile,
        bucket = "openradar-24h",
        opts = list(
          base_url = "s3.waw3-1.cloudferro.com",
          region = ""
        )
      )
    )
    if (length(pvols) < 1) {
      browser()
    }

    pvol <- merge_pvols(pvols)
  } else {
    scans <- purrr::map(
      keys_selected,
      ~ aws.s3::s3read_using(
        object = .x,
        read_scan,
        bucket = "openradar-24h",
        opts = list(
          base_url = "s3.waw3-1.cloudferro.com",
          region = ""
        )
      )
    )
    # TODO merge german scans
    # TODO check mtgud many scans

    pvol <- list_to_pvol(
      scans,
      time = lubridate::floor_date(time, '5 mins'),
      radar = radar,
      source = glue::glue("NOD:{radar},CMT:constructed from opera ord")
    )
  }
  return(pvol)
}


if (F) {
  time <- Sys.time()
  aa <- aws.s3::get_bucket_df(
    "openradar-24h",
    prefix = glue::glue(
      '{format(time,"%Y/%m/%d", tz="UTC")}/'
    ),
    base_url = "s3.waw3-1.cloudferro.com",
    region = "",
    max = Inf
  )
  gsub('@.*', '', aa$Key) |> unique()
  list(
    BE = c("behel", "bejab", "bewid"),
    CH = c("chalb", "chdol", "chlem", "chppm", "chwei"),
    CZ = c("czbrd", "czska"),
    DE = c(
      "deasb",
      "deboo",
      "dedrs",
      "deeis",
      "deess",
      "defbg",
      "defld",
      "dehnr",
      "deisn",
      "demem",
      "deneu",
      "denhb",
      "deoft",
      "depro",
      "deros",
      "detur",
      "deumd"
    ),
    DK = c("dkbor", "dkrom", "dksam", "dksin", "dkste"),
    EE = "eesur",
    FI = c(
      "fianj",
      "fikan",
      "fikau",
      "fikes",
      "fikor",
      "fikuo",
      "filuo",
      "finur",
      "fipet",
      "fiuta",
      "fivih",
      "fivim"
    ),
    FR = c(
      "frabb",
      "fraja",
      "frale",
      "frave",
      "frbla",
      "frbol",
      "frbor",
      "frbou",
      "frcae",
      "frcol",
      "frgre",
      "frmcl",
      "frmom",
      "frmtc",
      "frnan",
      "frnim",
      "frniz",
      "fropo",
      "frpla",
      "frtou",
      "frtre",
      "frtro"
    ),
    HR = c("hrbil", "hrdeb", "hrgra", "hrpun", "hrulj"),
    IE = c("iedub", "iedub", "iesha"),
    IS = c("isbjo", "iskef", "isska"),
    LT = c("ltlau", "ltvil"),
    MT = "mtgud",
    NL = c("nldhl", "nlhrw"),
    NO = c(
      "noand",
      "nober",
      "nobml",
      "nohas",
      "nohfj",
      "nohgb",
      "nohur",
      "norsa",
      "norsg",
      "norst",
      "nosmn",
      "nosta"
    ),
    PL = c(
      "plbrz",
      "plgdy",
      "plgsa",
      "plleg",
      "plpas",
      "plpoz",
      "plram",
      "plrze",
      "plswi",
      "pluzr"
    ),
    RO = c("robar", "robob", "robuc", "rocra", "romed", "roora", "rotim"),
    SE = c(
      "seang",
      "seatv",
      "sebaa",
      "sehem",
      "sehuv",
      "sekaa",
      "sekrn",
      "sella",
      "seoer",
      "seosd",
      "sevax"
    ),
    SI = c("silis", "sipas")
  )
}
