# getRad (development version)

* A bug (#101) in `get_vpts()` was fixed that caused the function to only return the first day of an interval, regardless of the length of the interval. (#105)

# getRad 0.2.0

* New function `get_weather_radars()` retrieves metadata for OPERA weather radars (#15, #54).
* New function `get_vpts()` downloads vertical profile time series from the [Aloft bucket](https://aloftdata.eu/browse/) and [RMI](https://opendata.meteo.be/geonetwork/srv/eng/catalog.search#/metadata/RMI_DATASET_CROW) (#10, #53).
* New function `get_vpts_coverage()` fetches an overview table of the files available on the [Aloft bucket](https://aloftdata.eu/browse/) (#10) and RMI.
* `get_pvol()` now downloads polar volumes from NOAA (United States) (#55).
* Add Cecilia Nilsson and Alexander Tedeschi as contributors.

# getRad 0.1.0

* Initial package development.
* New function `get_pvol()` downloads polar volumes for 6 countries.
