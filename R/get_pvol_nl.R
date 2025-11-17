get_pvol_nl <- function(radar, time, ..., call = rlang::caller_env()) {
  #  Convert radar names into the dirname and version used by the KNMI data platform.
  mapped_radar <-
    radar_recode(
      radar,
      "nlhrw" = "radar_volume_full_herwijnen",
      "nldhl" = "radar_volume_denhelder",
      call = call
    )
  version_radar <- radar_recode(
    radar,
    "nlhrw" = "1.0",
    "nldhl" = "2.0",
    call = call
  )
  # This request generate the temporary download url where the polar volume file can be retrieved
  resp <- tryCatch(
    httr2::request("https://api.dataplatform.knmi.nl/open-data/v1/datasets/") |>
      httr2::req_url_path_append(
        mapped_radar,
        "versions",
        version_radar,
        "files"
      ) |>
      req_user_agent_getrad() |>
      httr2::req_url_path_append(
        glue::glue(getOption(
          "getRad.nl_file_format",
          "RAD_{radar_recode(radar, 'nlhrw'='NL62','nldhl'='NL61')}_VOL_NA_{strftime(time,'%Y%m%d%H%M', tz='UTC')}.h5"
        ))
      ) |>
      httr2::req_url_path_append("/url") |>
      req_retry_getrad(max_tries = 5) |>
      httr2::req_headers(Authorization = get_secret("nl_api_key")) |>
      httr2::req_perform(error_call = call),
    httr2_http_403 = function(cnd) {
      cli::cli_abort(
        c(
          "There was an authorization error. You may have used an invalid API
           key.",
          "i" = "Please check if you set the correct {.val nl_api_key} with
                 {.code get_secret(\"nl_api_key\")}."
        ),
        cnd = cnd,
        class = "getRad_error_get_pvol_nl_authorization_failure",
        call = call
      )
    }
  )
  # This request retrieves the file
  pvol <- withr::with_tempfile("file", fileext = ".h5", {
    req <- httr2::resp_body_json(resp)$temporaryDownloadUrl |>
      httr2::req_url(req = resp$request) |>
      httr2::req_headers(Authorization = NULL) |>
      httr2::req_perform(path = file, error_call = call)
    # Dutch files need to be converted to the odim format
    converter <- getOption("getRad.nl_converter", "KNMI_vol_h5_to_ODIM_h5")
    if (!file.exists(converter)) {
      converter <- Sys.which(converter)
    }
    if (converter == "") {
      cli::cli_abort(
        c(
          "The program to convert KNMI data to the ODIM format can't be found.",
          "i" = "The source code for this binary can be obtained from {.file
                 {system.file(\"extra/KNMI_vol_h5_to_ODIM_h5.c\",
                 package=\"getRad\")}}.",
          "i" = "Please compile the binary and include it in the search path as
                 a program named {.val KNMI_vol_h5_to_ODIM_h5}. On linux systems
                 this can be done with the command {.code h5cc
                 KNMI_vol_h5_to_ODIM_h5.c -o KNMI_vol_h5_to_ODIM_h5}. If another
                 name is used or the program is not in the search path, use
                 options to locate the program ({.code
                 options(getRad.nl_converter=\"\")})."
        ),
        class = "getRad_error_no_nl_converter_found",
        call = call
      )
    }
    pvol_path <- paste0(req$body, ".odim.h5")
    system(paste(converter, pvol_path, req$body))
    bioRad::read_pvolfile(pvol_path, ...)
  })
  return(pvol)
}
