# Get VPTS data from RMI

Get VPTS data from
[RMI_DATASET_CROW](https://opendata.meteo.be/geonetwork/srv/eng/catalog.search#/metadata/RMI_DATASET_CROW).

## Usage

``` r
get_vpts_rmi(radar_odim_code, rounded_interval)
```

## Arguments

- radar_odim_code:

  Radar ODIM code.

- rounded_interval:

  Interval to fetch data for, rounded to nearest day.

## Value

A tibble with VPTS data.
