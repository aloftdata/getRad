get_pvol_dk <- function(radar, time, ..., call = rlang::caller_env()) {
  req <- httr2::request(
    getOption(
      "getRad.dk_url",
      "https://dmigw.govcloud.dk/v1/radardata/download"
    )
  ) |>
    req_user_agent_getrad() |>
    httr2::req_url_path_append(
      glue::glue(getOption(
        "getRad.dk_file_format",
        "{radar}_{strftime(time,'%Y%m%d%H%M', tz='UTC')}.vol.h5"
      ))
    ) |>
    httr2::req_url_query(`api-key` = get_secret("dk_api_key")) |>
    httr2::req_perform(path = tempfile(fileext = ".h5"), error_call = call)
  pvol <- bioRad::read_pvolfile(req$body, ...)
  file.remove(req$body)
  return(pvol)
}
