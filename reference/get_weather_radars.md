# Get weather radar metadata

Gets weather radar metadata from
[OPERA](https://www.eumetnet.eu/wp-content/themes/aeron-child/observations-programme/current-activities/opera/database/OPERA_Database/index.html)
and/or
[NEXRAD](https://www.ncei.noaa.gov/products/radar/next-generation-weather-radar).

## Usage

``` r
get_weather_radars(source = c("opera", "nexrad"), use_cache = TRUE, ...)
```

## Arguments

- source:

  Source of the metadata. `"opera"`, `"nexrad"` or `"all"`. If not
  provided, `"opera"` is used.

- use_cache:

  Logical indicating whether to use the cache. Default is `TRUE`. If
  `FALSE` the cache is ignored and the file is fetched anew. This can
  also be useful if you want to force a refresh of the cache.

- ...:

  Additional arguments passed on to reading functions per source,
  currently not used.

## Value

A sf or tibble with weather radar metadata. In all cases the column
`source` is added to indicate the source of the data and `radar` to show
the radar identifiers used in other functions like
[`get_pvol()`](https://aloftdata.github.io/getRad/reference/get_pvol.md)
and
[`get_vpts()`](https://aloftdata.github.io/getRad/reference/get_vpts.md).

## Details

The source files for this function are:

- For `opera`:
  [OPERA_RADARS_DB.json](http://eumetnet.eu/wp-content/themes/aeron-child/observations-programme/current-activities/opera/database/OPERA_Database/OPERA_RADARS_DB.json)
  (main/current) and
  [OPERA_RADARS_ARH_DB.json](http://eumetnet.eu/wp-content/themes/aeron-child/observations-programme/current-activities/opera/database/OPERA_Database/OPERA_RADARS_ARH_DB.json)
  (archive). A column `origin` is added to indicate which file the
  metadata were derived from.

- For `nexrad`:
  [nexrad-stations.txt](https://www.ncei.noaa.gov/access/homr/file/nexrad-stations.txt).

## Examples

``` r
if (FALSE) { # interactive()
# Get radar metadata from OPERA
get_weather_radars(source = "opera")

# Get radar metadata from NEXRAD
get_weather_radars(source = "nexrad")
}
```
