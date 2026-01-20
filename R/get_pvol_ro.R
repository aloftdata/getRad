get_pvol_ro <- function(radar, time, ..., call = rlang::caller_env()) {
  params <- c("KDP", "RhoHV", "V", "ZDR", "dBZ") # Height and dBR are images and not scans and thus should not be read
  urls <- glue::glue(
    "https://opendata.meteoromania.ro/radar/{toupper(substr(radar,3,5))}/{toupper(substr(radar,3,5))}_{strftime(time,'%Y%m%d%H%M', tz='UTC')}0200{params}.hdf"
  )
  tryCatch(
    read_pvol_from_url_per_param(urls, param = "all", call = call),
    error = function(cnd) {
      urls <- glue::glue(
        "https://opendata.meteoromania.ro/radar/{toupper(substr(radar,3,5))}/{toupper(substr(radar,3,5))}_{strftime(time,'%Y%m%d%H%M', tz='UTC')}0300{params}.hdf"
      )
      if (
        rlang::has_name(cnd, "parent") && inherits(cnd$parent, "httr2_http_404")
      ) {
        read_pvol_from_url_per_param(urls, param = "all", call = call)
      } else {
        return(cnd)
      }
    }
  )
}
