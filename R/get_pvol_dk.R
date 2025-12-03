get_pvol_dk <- function(radar, time, ..., call = rlang::caller_env()) {
  withr::with_file("file.h5", {
    req <- httr2::request(
      getOption(
        "getRad.dk_url",
        "https://opendataapi.dmi.dk/v1/radardata/download"
      )
    ) |>
      req_user_agent_getrad() |>
      httr2::req_url_path_append(
        glue::glue(getOption(
          "getRad.dk_file_format",
          "{radar}_{strftime(time,'%Y%m%d%H%M', tz='UTC')}.vol.h5"
        ))
      ) |>
      httr2::req_perform(path = "file.h5", error_call = call)
    pvol <- bioRad::read_pvolfile(req$body, ...)
  })
  return(pvol)
}
