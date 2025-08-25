get_pvol_se <- function(radar, time, ..., call = rlang::caller_env()) {
  radar_name <- radar_recode(radar,
    call = call,
    "seang" = "angelholm",
    "seatv" = "atvidaberg",
    "sebaa" = "balsta",
    "sehem" = "hemse",
    "sehuv" = "hudiksvall",
    "sekaa" = "karlskrona",
    "sekrn" = "kiruna",
    "selek" = "leksand",
    "sella" = "lulea",
    "seoer" = "ornskoldsvik",
    "seosd" = "ostersund",
    "sevax" = "vara"
  )
  url_path <- glue::glue(
    getOption(
      "getRad.se_path_format",
      default = '/area/{radar_name}/product/qcvol/{year}/{month}/{day}/radar_{radar_name}_qcvol_{datetime}.h5'
    ),
    year = lubridate::year(time),
    month = lubridate::month(time),
    day = lubridate::day(time),
    datetime = strftime(time, "%Y%m%d%H%M", tz="UTC")
  )
  pvol <- withr::with_tempfile("file", {
    req <- withCallingHandlers(
      httr2::request(
        getOption(
          "getRad.se_url",
          default = "https://opendata-download-radar.smhi.se/api/version/latest"
        )
      ) |>
        req_user_agent_getrad() |>
        (\(request){
        if("getRad.se_path_format" %in% options()){
          httr2::req_url_path_append(request, getOption("getRad.se_path_format"))
        } else {
          httr2::req_url_path_append(request,
                                     "area",
                                     radar_name,
                                     "product",
                                     "qcvol",
                                     lubridate::year(time),
                                     lubridate::month(time),
                                     lubridate::day(time),
                                     glue::glue(
                                       "radar_{radar_name}_qcvol_{datetime}.h5",
                                       datetime = strftime(time,
                                                           "%Y%m%d%H%M",
                                                           tz = "UTC"))
          )
          }
          }
        )() |>
        httr2::req_retry(
          max_tries = 5,
          # this should catch curl streaming errors
          retry_on_failure = T,
          # this should catch not downloading a valid h5 file
          is_transient = function(resp) {
            # for example when a file out of date range is requested
            if (httr2::resp_status(resp) == 404) {
              return(FALSE)
            }
            r <- inherits(try(
              {
                f <- rhdf5::H5Fopen(resp$body)
                rhdf5::H5Fclose(f)
              },
              silent = T
            ), "try-error")
            r
          }
        ) |>
        httr2::req_perform(path = file, error_call = call),
      httr2_http_404 = function(cnd) {
        cli::cli_abort(
          c(
            x = "No polar volume data could be found for {.val {radar}} at time {.val {time}}",
            i = "Volume data in Sweden is only available for 24 hours",
            i = "If the requested time is within the last 24 hours the error might relate to a server outage or package problem"
          ),
          parent = cnd, call = call, url = url, class = "getRad_error_get_pvol_se_data_not_found"
        )
      }
    )
    bioRad::read_pvolfile(req$body, ...)
  })
  pvol
}
