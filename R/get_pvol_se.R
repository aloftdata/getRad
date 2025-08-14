get_pvol_se_radar_mapping <- c(
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

get_pvol_se <- function(radar, time, ..., call = rlang::caller_env()) {
  if (!radar %in% names(get_pvol_se_radar_mapping)) {
    cli::cli_abort(
      c(
        x = "The radar {.val {radar}} is not found in the mapping for Swedish radars",
        i = "Most likely this means an invalid odim code has been provided"
      ),
      call = call, class = "getRad_error_get_pvol_se_radar_not_found"
    )
  }
  radar_name <- get_pvol_se_radar_mapping[radar]
  url <- glue::glue(
    getOption(
      "getRad.se_path_format",
      '/area/{radar_name}/product/qcvol/{lubridate::year(time)}/{lubridate::month(time)}/{lubridate::day(time)}/radar_{radar_name}_qcvol_{strftime(time, "%Y%m%d%H%M", tz="UTC" )}.h5'
    )
  )
  pvol<-withr::with_tempfile("file", {
    req <- withCallingHandlers(
      httr2::request(
        getOption(
          "getRad.se_url",
          "https://opendata-download-radar.smhi.se/api/version/latest"
        )
      ) |>
        req_user_agent_getrad() |>
        httr2::req_url_path_append(url) |>
        httr2::req_retry(
          max_tries = 5,
          # this should catch curl streaming errors
          retry_on_failure = T,
          # this should catch not downloading a valid h5 file
          is_transient = function(resp) {
            # for example when a file out of date range is requested
            if(httr2::resp_status(resp)==404)
            {
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
