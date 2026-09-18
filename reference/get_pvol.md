# Get polar volume (PVOL) data from supported sources

Gets polar volume data from supported sources and returns it as a (list
of) [polar volume
objects](http://adriaandokter.com/bioRad/reference/summary.pvol.md). The
source is automatically detected based on the provided `radar`.

## Usage

``` r
get_pvol(radar = NULL, datetime = NULL, ...)
```

## Arguments

- radar:

  Name of the radar (odim code) as a character string (e.g. `"nlhrw"` or
  `"fikor"`).

- datetime:

  Either:

  - A single [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html),
    for which the most representative data file is downloaded. In most
    cases this will be the time before.

  - A
    [`lubridate::interval()`](https://lubridate.tidyverse.org/reference/interval.html)
    or two [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html),
    between which all data files are downloaded.

- ...:

  Additional arguments used to select reading functions or passed on to
  reading functions. For example:

  - `use_opera_ord = TRUE` if you want to force using the Opera open
    radar data over specific reading functions. Generally speaking this
    data is less suitable for biological analysis. Note this data is
    only available for 24 hours.

  - `param = c("DBZH", "VRADH")` to the
    [`bioRad::read_pvolfile()`](http://adriaandokter.com/bioRad/reference/read_pvolfile.md).

## Value

Either a polar volume or a list of polar volumes. See
[`bioRad::summary.pvol()`](http://adriaandokter.com/bioRad/reference/summary.pvol.md)
for details.

## Details

For more details on supported sources, see
[`vignette("supported_sources")`](https://aloftdata.github.io/getRad/articles/supported_sources.md).
Within supported countries there might also be temporal restrictions on
the radars that are operational. For example, radars with the `status`
`0` in `get_weather_radars("opera")` are currently not operational.

For European countries where a dedicated archive exist this data is
used. Alternatively data is downloaded from the Opera open radar data
archive. This data is frequently filtered. Use the argument
`use_opera_ord = TRUE` if you want to force using the Opera open radar
data.

Not all radars in the nexrad archive can be read successfully. Radars
associated with the Terminal Doppler Weather Radar (TDWR) program can
not be read. These can be identified using the `stntype` column in
`get_weather_radars("nexrad")`.

## Examples

``` r
if (FALSE) { # interactive()
# Get PVOL data for a single radar and datetime
get_pvol("deess", as.POSIXct(Sys.Date()))

# Get similar data from the opera open radar data
get_pvol("deess", as.POSIXct(Sys.Date()), use_opera_ord=TRUE)

# Get PVOL data for multiple radars and a single datetime
get_pvol(
  c("deess", "dehnr", "fianj", "czska", "KABR"),
  as.POSIXct(Sys.Date())
)

get_pvol("fianj",
  as.POSIXct("2012-05-11 23:00", tz = "UTC"),
  param = "DBZH"
)
}
```
