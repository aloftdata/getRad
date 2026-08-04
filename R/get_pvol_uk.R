get_pvol_uk <- function(radar, time, pulse_type=c("lp", "sp"), ..., call = rlang::caller_env()) {
  uk_odim_map<-c(ukcle = "clee-hill",
                 ukham = "hameldon-hill",
                 ukche = "chenies",
                 ukcas = "castor-bay",
                 ukpre = "predannack",
                 uking = "ingham",
                 ukcyg = "crug-y-gorrllwyn",
                 ukhhd = "holehead",
                 ukjer = "jersey",
                 ukdud = "dudwick",
                 uklew = "druima-starraig",
                 ukcob = "cobbacombe",
                 ukdea = "deanhill",
                 ukthu = "thurnham",
                 ukmun = "munduff-hill",
                 ukhmy = "high-moorsley")
  uk_odim_map_radar_number<-c(ukcle = 3,
                 ukham = 4,
                 ukche = 5,
                 ukcas = 7,
                 ukpre = 8,
                 uking = 9,
                 ukcyg = 10,
                 ukhhd = 18,
                 ukjer = 12,
                 ukdud = 14,
                 uklew = 15,
                 ukcob = 16,
                 ukdea = 21,
                 ukthu = 20,
                 ukmun = 19,
                 ukhmy = 23)
  pulse_type<- rlang::arg_match(pulse_type)
  # TODO wardon-hill code 11 no odim coe
  withr::with_file("file.h5", {
    req <- httr2::request(glue::glue(
      "https://ncas-radar-o.s3-ext.jc.rl.ac.uk/uk-wsr-visualizer-public/ukmo-nimrod/pvol/{uk_odim_map[radar]}/{format(time,'%Y/%m/%d')}/{pulse_type}/{format(time,'%Y%m%d')}_polar_pl_radar{sprintf('%02d', uk_odim_map_radar_number[radar])}_aggregate_{pulse_type}_{format(time,'%H%M')}.h5"
    )) |>
      req_user_agent_getrad() |>
      httr2::req_perform(path = "file.h5", error_call = call)
    pvol <- bioRad::read_pvolfile(req$body, ...)
  })
  return(pvol)
}



