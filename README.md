
<!-- README.md is generated from README.Rmd. Please edit that file -->

``` r

  library(envReport)
  library(envImport)
#> The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
#> which was just loaded, will retire in October 2023.
#> Please refer to R-spatial evolution reports for details, especially
#> https://r-spatial.org/r/2023/05/15/evolution4.html.
#> It may be desirable to make the sf package available;
#> package maintainers should consider adding sf to Suggests:.
#> The sp package is now running under evolution status 2
#>      (status 2 uses the sf package in place of rgdal)
  library(magrittr)
```

# envImport

<!-- badges: start -->
<!-- badges: end -->

The goal of envImport is to obtain, and assemble, environmental data
from disparate data sources, for a geographic area of interest. As
little filtering as possible occurs when obtaining data and envImport
does *not* aim to clean, filter or tidy the data (see
[envClean](https://acanthiza.github.io/envClean/) for help there).

## Installation

You can install the development version of envImport from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Acanthiza/envImport")
```

## Supported data sources

`data_name` = ‘data source’. Data sources are (usually) obvious sources
of data. Examples are the Global Biodiversity Infrastructure Facility
([GBIF](https://www.gbif.org/)) or Terrestrial Ecosystems Network
([TERN](https://www.tern.org.au/)). The 9 data sources currently
supported are (also see `envImport::data_map`):

- BDBSA: Biological databases of South Australia
- EGIS: ‘Supertable’ from the environmental databases of South Australia
- TERN: Terrestrial ecosystem network
- ALIS: Arid lands information systems
- BCM: Bushland condition monitoring
- PTP: Paddock tree project
- NVB: DEW Native Vegetation Branch
- GBIF: Global biodiversity information facility
- BA: BirdLife Australia

## General workflow

### `data_map`

The data_map (see table @ref(tab:dataMap)) provides a mapping from
original data sources to the desired columns in the assembled data set.

``` r

  knitr::kable(data_map
               , caption = "Data map of desired columns in the assembled data (columns) and names of columns in the original data (rows)"
               )
```

| data_name | order | days | site        | date             | lat             | long             | original_name            | nsx          | number      | survey_nr | survey     | ind                        | rel_nr   | rel_metres                    | sens         | lifeform  | lifespan | cover            | cover_code | quad_x       | quad_y       | epbc_status     | npw_staus        | desc                                                             |
|:----------|------:|-----:|:------------|:-----------------|:----------------|:-----------------|:-------------------------|:-------------|:------------|:----------|:-----------|:---------------------------|:---------|:------------------------------|:-------------|:----------|:---------|:-----------------|:-----------|:-------------|:-------------|:----------------|:-----------------|:-----------------------------------------------------------------|
| BDBSA     |     1 |  700 | PATCHID     | VISITDATE        | LATITUDE        | LONGITUDE        | SPECIES                  | NSXCODE      | NUMOBSERVED | SURVEYNR  | SURVEYNAME | ISINDIGENOUSFLAG           | RELIABNR | rel_metres                    | NA           | MUIRCODE  | LIFESPAN | COVER            | COVCODE    | VEGQUADSIZE1 | VEGQUADSIZE2 | ESACTSTATUSCODE | NPWACTSTATUSCODE | Biological databases of South Australia                          |
| EGIS      |     2 |  700 | EGISCODE    | SIGHTINGDATE     | LATITUDE        | LONGITUDE        | SPECIES                  | NSXCODE      | NUMOBSERVED | SURVEYNR  | SURVEYNAME | ISINDIGENOUSFLAG           | RELIABNR | rel_metres                    | DISTRIBNDESC | NA        | NA       | NA               | NA         | NA           | NA           | ESACTSTATUSCODE | NPWACTSTATUSCODE | ‘Supertable’ from the environmental databases of South Australia |
| TERN      |     3 |   60 | site_unique | visit_start_date | latitude        | longitude        | herbarium_determination  | NA           | NA          | NA        | NA         | NA                         | NA       | NA                            | NA           | MUIRCODE  | NA       | COVER            | NA         | quadX        | quadY        | NA              | NA               | Terrestrial ecosystem network                                    |
| ALIS      |     4 |   60 | SITENUMBER  | SurveyDate       | LATITUDE        | LONGITUDE        | LegacyName               | NSXCode      | NA          | NA        | NA         | NA                         | NA       | NA                            | NA           | Lifeform  | LIFESPAN | Cover            | NA         | NA           | NA           | NA              | NA               | Arid lands information systems                                   |
| BCM       |     5 |   60 | SITE_ID     | ASSESSMENT_DATE  | LATITUDE        | LONGITUDE        | Species                  | Old_NSX_Code | NA          | NA        | NA         | isIndigenous               | NA       | NA                            | NA           | NA        | NA       | NA               | NA         | X_DIM        | Y_DIM        | NA              | NA               | Bushland condition monitoring                                    |
| PTP       |     6 |  700 | PlantDataID | Obs_Date         | LATITUDE        | LONGITUDE        | Scientific_name_original | NSXCODE      | NA          | NA        | NA         | Native_Introduced_original | NA       | NA                            | NA           | Life_form | NA       | NA               | COVCODE    | NA           | NA           | NA              | NA               | Paddock tree project                                             |
| NVB       |     7 | 1000 | path        | date             | lat             | lon              | Spp                      | NA           | NA          | NA        | NA         | NA                         | NA       | NA                            | NA           | NA        | NA       | NA               | NA         | NA           | NA           | NA              | NA               | DEW Native Vegetation Branch                                     |
| GBIF      |     8 |   90 | gbifID      | eventDate        | decimalLatitude | decimalLongitude | species                  | organismID   | NA          | NA        | NA         | NA                         | NA       | coordinateUncertaintyInMeters | NA           | NA        | NA       | organismQuantity | NA         | NA           | NA           | NA              | NA               | Global biodiversity information facility                         |
| BA        |     9 |  365 | PATCHID     | SIGHTINGDATE     | LATITUDE        | LONGITUDE        | SPECIES as supplied      | NSXCODE      | NUMOBSERVED | SURVEYNR  | SURVEYNAME | NA                         | RELIABNR | rel_metres                    | NA           | NA        | NA       | NA               | NA         | NA           | NA           | NA              | NA               | BirdLife Australia                                               |

Data map of desired columns in the assembled data (columns) and names of
columns in the original data (rows)

### `get_x`

`get_x` functions get data from the data source `x`. Results are always
saved to disk (as getting data can be slow). When run again, they load
from the saved file by default. If available, `get_x` functions use any
R packages and functions provided by the data source (e.g. GBIF provides
`rgbif` \[@R-rgbif;@rgbif2017\] and TERN provides `ausplotsR`
\[@R-ausplotsR\]). The first arguments to `get_x` functions are always:

- `aoi`: an area of interest, provided as simple feature. `get_x` will
  turn any `aoi` into a bounding box and convert to coordinates
  appropriate for data source `x`. \[Ed: is `aoi` always required?\]
- `save_file`: a path to save the results to. The default (`NULL`) leads
  to the file `here::here("out", "ds", "x.rds")` being created and used
  as `save_file`. `ds` is for ‘data source’. While the saved file is
  usually `x.rds`, in some instances it follows the format and naming of
  the download from `x` (e.g. GBIF data comes in a `.zip` file named by
  the corresponding download key).
- `get_new`: an override to force `get_x` to requery the data source,
  even if save_file already exists
- `...`: the dots are passed to any underlying ‘native’ function, such
  as `rgbif::occ_download()` or `ausplotsR::get_ausplots()`

May of the `get_x` functions will only work within `DEW`.

### `make_x`

`make_x` functions take the results from `get_x` functions and prepare
them for assembly. This usually involves:

- selecting columns (this is really just aesthetic and helps with
  debugging when there are tens of columns returned)
- munging a column or two to align with the data_map. For example, the
  raw TERN plant data obtained via get_tern is point intercept data. The
  `make_tern` function transforms the raw data into projected foliage
  cover data (making use of the `ausplotsR::species_table()` function)
