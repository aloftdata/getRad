get_pvol_sk <- function(radar, time, ..., call = rlang::caller_env()) {
  params <- c(
    "PARZ" = "KDP",
    "PAQZ" = "PhiDP",
    "PALZ" = "RhoHV",
    "PAHZ" = "V",
    "PAIZ" = "W",
    "PAKZ" = "ZDR",
    "PAGZ" = "dBZ",
    "PAJZ" = "dBuZ"
  ) # Height and dBR are images and not scans and thus should not be read
  radar_number <- c("sklaz" = 70, "skkub" = 60, "skjav" = 41, "skkoj" = 51)
  urls <- glue::glue(
    "https://opendata.shmu.sk/meteorology/weather/radar/volume/{tolower(radar)}/{params}/{strftime(time,'%Y%m%d', tz='UTC')}/T_{names(params)}{radar_number[radar]}_C_LZIB_{strftime(time,'%Y%m%d%H%M%S', tz='UTC')}.hdf"
  )
  read_pvol_from_url_per_param(urls, param = "all", call = call)
}
