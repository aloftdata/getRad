get_pvol_hochficht <- function(radar, time, ..., call = rlang::caller_env()) {
  url <- glue::glue(
    getOption(
      "getRad.at_hochficht_url",
      default = "https://public.hub.geosphere.at/datahub/resources/radar_volumen_hochficht-v1-5min/filelisting/WXRHOF_{strftime(time,'%Y%m%d%H%M', tz='UTC')}.hdf"
    )
  )
  pvol <- withr::with_tempfile("file", fileext = ".h5", {
    tryCatch(
      req <-
        httr2::request(url) |>
        req_user_agent_getrad() |>
        httr2::req_perform(path = file, error_call = call),
      httr2_http_403 = function(cnd) {
        cli::cli_abort(
          c(
            "There is data found for download.",
            "i" = "Data for the Hochficht radar is only available for the last three days."
          ),
          cnd = cnd,
          class = "getRad_error_get_pvol_at_no_data",
          call = call
        )
      }
    )
    bioRad::read_pvolfile(file, ...)
  })
  return(pvol)
}
