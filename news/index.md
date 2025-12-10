# Changelog

## getRad (development version)

- Make error messages more consistent
  ([\#146](https://github.com/aloftdata/getRad/issues/146)).
- Restore access to Estonian data.
- Prevent leaving temporary files for downloading Dutch polar volume
  data ([\#148](https://github.com/aloftdata/getRad/issues/148)).
- Denmark does not require authentication anymore, removed from the
  package ([\#154](https://github.com/aloftdata/getRad/issues/154)).
- Account for url change of opera database
  ([\#160](https://github.com/aloftdata/getRad/issues/160)).

## getRad 0.2.3

CRAN release: 2025-10-14

- Improve error for requesting German data out of temporal restrictions
  ([\#131](https://github.com/aloftdata/getRad/issues/131)).
- Start using the air formatter
  ([\#128](https://github.com/aloftdata/getRad/issues/128)).
- Do not fail but rather warn when csv is missing from repository
  ([\#136](https://github.com/aloftdata/getRad/issues/136)).
- In `get_pvol` correct where attributes for German data causing
  incorrect `vp` heights
  ([\#139](https://github.com/aloftdata/getRad/issues/139)).

## getRad 0.2.2

CRAN release: 2025-09-29

- Support downloading Slovakian polar volume data
  ([\#124](https://github.com/aloftdata/getRad/issues/124)).
- Add retry attempts to `get_weather_radars` for NEXRAD to prevent
  failure ([\#116](https://github.com/aloftdata/getRad/issues/116)).
- Update of NEXRAD url
  ([\#118](https://github.com/aloftdata/getRad/issues/118)).
- Fix CRAN warning where cache was not cleaned after tests
  ([\#122](https://github.com/aloftdata/getRad/issues/122)).
- Resolve `withr` error for Danish radars.

## getRad 0.2.1

CRAN release: 2025-08-25

- A bug ([\#101](https://github.com/aloftdata/getRad/issues/101)) in
  [`get_vpts()`](https://aloftdata.github.io/getRad/reference/get_vpts.md)
  was fixed that caused the function to only return the first day of an
  interval, regardless of the length of the interval
  ([\#105](https://github.com/aloftdata/getRad/issues/105)).
- Support downloading Swedish polar volume data
  ([\#96](https://github.com/aloftdata/getRad/issues/96)).
- Support downloading Romanian polar volume data
  ([\#104](https://github.com/aloftdata/getRad/issues/104)).
- How attribute is now present in Czech data
  ([\#102](https://github.com/aloftdata/getRad/issues/102)).
- Use `withr` to prevent files being left in temporary directories
  ([\#98](https://github.com/aloftdata/getRad/issues/98)).

## getRad 0.2.0

CRAN release: 2025-07-16

- New function
  [`get_weather_radars()`](https://aloftdata.github.io/getRad/reference/get_weather_radars.md)
  retrieves metadata for OPERA weather radars
  ([\#15](https://github.com/aloftdata/getRad/issues/15),
  [\#54](https://github.com/aloftdata/getRad/issues/54)).
- New function
  [`get_vpts()`](https://aloftdata.github.io/getRad/reference/get_vpts.md)
  downloads vertical profile time series from the [Aloft
  bucket](https://aloftdata.eu/browse/) and
  [RMI](https://opendata.meteo.be/geonetwork/srv/eng/catalog.search#/metadata/RMI_DATASET_CROW)
  ([\#10](https://github.com/aloftdata/getRad/issues/10),
  [\#53](https://github.com/aloftdata/getRad/issues/53)).
- New function
  [`get_vpts_coverage()`](https://aloftdata.github.io/getRad/reference/get_vpts_coverage.md)
  fetches an overview table of the files available on the [Aloft
  bucket](https://aloftdata.eu/browse/)
  ([\#10](https://github.com/aloftdata/getRad/issues/10)) and RMI.
- [`get_pvol()`](https://aloftdata.github.io/getRad/reference/get_pvol.md)
  now downloads polar volumes from NOAA (United States)
  ([\#55](https://github.com/aloftdata/getRad/issues/55)).
- Add Cecilia Nilsson and Alexander Tedeschi as contributors.

## getRad 0.1.0

- Initial package development.
- New function
  [`get_pvol()`](https://aloftdata.github.io/getRad/reference/get_pvol.md)
  downloads polar volumes for 6 countries.
