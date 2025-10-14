# getRad 0.2.3

* Improve error for requesting German data out of temporal restrictions (#131).
* Start using the air formatter (#128).
* Do not fail but rather warn when csv is missing from repository (#136).
* In `get_pvol` correct where attributes for German data causing incorrect `vp` heights (#139). 

# getRad 0.2.2

* Support downloading Slovakian polar volume data (#124).
* Add retry attempts to `get_weather_radars` for NEXRAD to prevent failure (#116).
* Update of NEXRAD url (#118).
* Fix CRAN warning where cache was not cleaned after tests (#122).
* Resolve `withr` error for Danish radars.

# getRad 0.2.1

* A bug (#101) in `get_vpts()` was fixed that caused the function to only return the first day of an interval, regardless of the length of the interval (#105).
* Support downloading Swedish polar volume data (#96).
* Support downloading Romanian polar volume data (#104).
* How attribute is now present in Czech data (#102).
* Use `withr` to prevent files being left in temporary directories (#98).

# getRad 0.2.0

* New function `get_weather_radars()` retrieves metadata for OPERA weather radars (#15, #54).
* New function `get_vpts()` downloads vertical profile time series from the [Aloft bucket](https://aloftdata.eu/browse/) and [RMI](https://opendata.meteo.be/geonetwork/srv/eng/catalog.search#/metadata/RMI_DATASET_CROW) (#10, #53).
* New function `get_vpts_coverage()` fetches an overview table of the files available on the [Aloft bucket](https://aloftdata.eu/browse/) (#10) and RMI.
* `get_pvol()` now downloads polar volumes from NOAA (United States) (#55).
* Add Cecilia Nilsson and Alexander Tedeschi as contributors.

# getRad 0.1.0

* Initial package development.
* New function `get_pvol()` downloads polar volumes for 6 countries.
