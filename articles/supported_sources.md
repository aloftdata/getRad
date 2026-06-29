# Supported sources

## Polar volume (PVOL) data

[`get_pvol()`](https://aloftdata.github.io/getRad/reference/get_pvol.md)
can read polar volume data from a number of sources. See the map for
details.

To get an overview of weather radar metadata, use
[`get_weather_radars()`](https://aloftdata.github.io/getRad/reference/get_weather_radars.md).

## Vertical profile time series (VPTS) data

[`get_vpts()`](https://aloftdata.github.io/getRad/reference/get_vpts.md)
can read vertical profile data from the online sources listed below.
These sources correspond to different processing pipelines where polar
volumes have been processed and filtered in different ways, see for
example [Shamoun-Baranes et
al. (2020)](https://doi.org/10.1175/BAMS-D-21-0196.1). Multiple sources
can contain data from from the same radar and time periods. For an
overview of the coverage available use
[`get_vpts_coverage()`](https://aloftdata.github.io/getRad/reference/get_vpts_coverage.md)
or explore the
[article](https://aloftdata.github.io/getRad/articles/vpts_coverage.html)
on the getRad webpage.
[`get_vpts()`](https://aloftdata.github.io/getRad/reference/get_vpts.md)
can also read locally stored files from either the Aloft or Dark Ecology
processing pipelines.

### Aloft: baltrad, uva or ecog-04003

Vertical profiles from European weather radar data that are stored on
the Aloft bucket. The `baltrad` source covers most radars and longest
time series, however data quality is more variable as polar volumes are
sometimes filtered for biological signals before processing to vertical
profiles. `uva` and `ecog-04003` are more curated datasets. See
[details](https://aloftdata.eu/faq/#what-data-are-in-the-bucket).

### rmi

Vertical profiles provided by the Royal Meteorological Institute of
Belgium (RMI). This dataset is restricted to Belgium and some radars in
the surrounding countries. See
[details](https://opendata.meteo.be/geonetwork/srv/eng/catalog.search#/metadata/RMI_DATASET_CROW).

### BirdCast

Vertical profiles from the US weather radar data (NEXRAD) processed
through the BirdCast pipeline and stored on the BirdCast bucket.
