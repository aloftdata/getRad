get_pvol_ro <- function(radar, time, ..., call = rlang::caller_env()) {
  params <- c("KDP", "RhoHV", "V", "ZDR", "dBZ") # Height and dBR are images and not scans and thus should not be read
  urls <- glue::glue(
    "https://opendata.meteoromania.ro/radar/{toupper(substr(radar,3,5))}/{toupper(substr(radar,3,5))}_{strftime(time,'%Y%m%d%H%M', tz='UTC')}0200{params}.hdf"
  )
  read_pvol_from_url_per_param(urls, param = "all", call = call)
}
