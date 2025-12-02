# Supported sources

## Polar volume (PVOL) data

[`get_pvol()`](https://aloftdata.github.io/getRad/reference/get_pvol.md)
can read polar volume data from a number of sources. See the map for
details.

To get an overview of weather radar metadata, use
[`get_weather_radars()`](https://aloftdata.github.io/getRad/reference/get_weather_radars.md).

## Vertical profile time series (VPTS) data

[`get_vpts()`](https://aloftdata.github.io/getRad/reference/get_vpts.md)
can read vertical profile data from the following sources. These sources
correspond to different processing pipelines. They can, for example, use
polar volumes that have been processed and filtered in different ways,
for more details see [Shamoun-Baranes et
al. (2020)](https://doi.org/10.1175/BAMS-D-21-0196.1). Multiple sources
can thus contain data from from the same radar and time periods. For an
overview of the coverage available use
[`get_vpts_coverage()`](https://aloftdata.github.io/getRad/reference/get_vpts_coverage.md)
or explore the
[article](https://aloftdata.github.io/getRad/articles/vpts_coverage.html)
on the getRad webpage.

### baltrad, uva, ecog-04003

These data are stored on the Aloft bucket. The `baltrad` source covers
most radars and longest time series, however data quality is more
variable as polar volumes are sometimes filtered for biological signals
before processing to vertical profiles. `uva` and `ecog-04003` are more
curated datasets. See
[details](https://aloftdata.eu/faq/#what-data-are-in-the-bucket).

### rmi

These data are provided by the Royal Meteorological Institute of Belgium
(RMI). This dataset is restricted to Belgium and some radars in the
surrounding countries. See
[details](https://opendata.meteo.be/geonetwork/srv/eng/catalog.search#/metadata/RMI_DATASET_CROW).
