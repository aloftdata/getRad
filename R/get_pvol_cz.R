# http://opendata.chmi.cz/meteorology/weather/radar/sites/ska/vol_z/hdf5/

get_pvol_cz <- function(radar, time, ..., call = rlang::caller_env()) {
  time_chr <- time_pos <- base <- resp <- NULL
  # All parameters are retrieved from separate files
  # Here all urls are generated
  params <- c("z", "u", "v", "w", "zdr", "rhohv", "phidp")
  urls <- glue::glue("http://opendata.chmi.cz/meteorology/weather/radar/sites/{substr(radar,3,5)}/vol_{params}/hdf5/")
  rlang::check_installed(
    c("lubridate", "tidyr", "xml2", "rhdf5"),
    "to read Czech radar data",
    call = call
  )
  res <- lapply(urls, function(x) {
    httr2::request(x) |>
      req_user_agent_getrad() |>
      httr2::req_perform(error_call = call) |>
      httr2::resp_body_html() |>
      xml2::xml_find_all("//a/@href") |>
      xml2::xml_text()
  })
  files_to_get <- data.frame(base = urls) |>
    dplyr::mutate(file = res) |>
    tidyr::unnest(file) |>
    dplyr::filter(file != "../") |>
    dplyr::mutate(
      time_chr = sub(".hdf", "", sub(".*_OKPR_", "", file))
    ) |>
    dplyr::mutate(
      time_pos = strptime(time_chr, "%Y%m%d%H%M%S", tz = "UTC")
    ) |>
    dplyr::filter(lubridate::`%within%`(
      time_pos,
      lubridate::interval(
        time,
        time + lubridate::minutes(5)
      )
    ))

  pvol<-read_pvol_from_url_per_param(paste0(files_to_get$base, files_to_get$file))

  pvol
}
