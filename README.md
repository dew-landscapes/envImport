
<!-- README.md is generated from README.Rmd. Please edit that file -->

``` r

  library(envReport)
  library(envImport)
  library(magrittr)
```

# envImport

<!-- badges: start -->
<!-- badges: end -->

The goal of envImport is to obtain, and make seamlessly useable,
environmental data from disparate data sources, for a geographic area of
interest.

## Installation

You can install the development version of envImport from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dew-landscapes/envImport")
```

## Supported data sources

`data_name` = ‘data source’. Data sources are (usually) obvious sources
of data. Examples are the Global Biodiversity Infrastructure Facility
([GBIF](https://www.gbif.org/)), Atlas of Living Australia
([ALA](https://www.ala.org.au/)) or Terrestrial Ecosystems Network
([TERN](https://www.tern.org.au/)). There are 12 data sources currently
supported (also see `envImport::data_map`):

- bdbsa: Biological databases of South Australia
- egis: Occurrence datasets from the environmental databases of South
  Australia (e.g. supertables)
- havplot: Harmonised Australian Vegetation Plot dataset
- tern: Terrestrial ecosystem network
- abbbs: Australian Bird and Bat Banding Scheme
- alis: Arid lands information system
- bcm: Bushland condition monitoring
- ptp: Paddock tree project
- nvb: DEW Native Vegetation Branch
- other: Other private datasets: SA Bird Atlas (UOA/Birds SA), Birdlife
  Australia Birdata portal, MLR Extra Bandicoot data, KI Post Fire Bird
  Monitoring, SA Seed Conservation Centre
- galah: Atlas of Living Australia
- gbif: Global biodiversity information facility

Four of these sources are publicly available (GBIF, ALA, HAVPlot and
TERN).

## General workflow

### `data_map`

The data_map (see table below) provides a mapping from original data
sources to the desired columns in the assembled data set.

| col            | gbif                                     | tern                          | galah                         | havplot                                                                                |
|:---------------|:-----------------------------------------|:------------------------------|:------------------------------|:---------------------------------------------------------------------------------------|
| data_name      | gbif                                     | tern                          | galah                         | havplot                                                                                |
| epsg           | 4326                                     | 4326                          | 4326                          | 4326                                                                                   |
| site           | gbifID                                   | site_unique                   | locationID                    | plotName                                                                               |
| date           | eventDate                                | visit_start_date              | eventDate                     | obsStartDate                                                                           |
| lat            | decimalLatitude                          | latitude                      | decimalLatitude               | decimalLatitude                                                                        |
| long           | decimalLongitude                         | longitude                     | decimalLongitude              | decimalLongitude                                                                       |
| original_name  | scientificName                           | species                       | scientificName                | scientificName                                                                         |
| common         | NA                                       | NA                            | vernacularName                | NA                                                                                     |
| nsx            | NA                                       | NA                            | organismID                    | NA                                                                                     |
| occ_derivation | occurrenceStatus                         | NA                            | occurrenceStatus              | abundanceValue                                                                         |
| quantity       | organismQuantity                         | NA                            | organismQuantity              | abundanceValue                                                                         |
| survey_nr      | NA                                       | NA                            | NA                            | NA                                                                                     |
| survey         | NA                                       | NA                            | datasetName                   | projectID                                                                              |
| ind            | NA                                       | NA                            | NA                            | NA                                                                                     |
| rel_metres     | coordinateUncertaintyInMeters            | NA                            | coordinateUncertaintyInMeters | coordinateUncertaintyInMetres                                                          |
| sens           | NA                                       | NA                            | NA                            | NA                                                                                     |
| lifeform       | NA                                       | lifeform                      | NA                            | NA                                                                                     |
| lifespan       | NA                                       | NA                            | NA                            | NA                                                                                     |
| cover          | NA                                       | cover                         | NA                            | cover                                                                                  |
| cover_code     | NA                                       | NA                            | NA                            | NA                                                                                     |
| height         | NA                                       | height                        | NA                            | NA                                                                                     |
| quad_metres    | NA                                       | quad_metres                   | NA                            | quad_metres                                                                            |
| epbc_status    | NA                                       | NA                            | NA                            | NA                                                                                     |
| npw_status     | NA                                       | NA                            | NA                            | NA                                                                                     |
| method         | samplingProtocol                         | NA                            | samplingProtocol              | abundanceMethod                                                                        |
| obs            | recordedBy                               | observer_veg                  | recordedBy                    | individualName                                                                         |
| denatured      | informationWithheld                      | NA                            | generalisationInMetres        | NA                                                                                     |
| kingdom        | kingdom                                  | kingdom                       | kingdom                       | kingdom                                                                                |
| desc           | Global biodiversity information facility | Terrestrial ecosystem network | Atlas of Living Australia     | Harmonised Australian Vegetation Plot dataset                                          |
| data_name_use  | GBIF                                     | TERN                          | ALA                           | HAVPlot                                                                                |
| url            | <https://www.gbif.org/>                  | <https://www.tern.org.au/>    | <https://www.ala.org.au/>     | <https://researchdata.edu.au/harmonised-australian-vegetation-dataset-havplot/1950860> |
| order          | 12                                       | 4                             | 11                            | 3                                                                                      |

Data map of desired columns in the assembled data (col) and names of
columns in the original data. Where a column name from the original data
source does not match columns in the original data source, the get_x
function has usually created a new column to better meet the
requirements of the final combined data set

### `get_x`

`get_x` functions get data from the data source `x`. Results are always
saved to disk (as getting data can be slow). When run again, they load
from the saved file by default. If available, `get_x` functions use any
R packages and functions provided by the data source (e.g. TERN provides
`ausplotsR` \[@R-ausplotsR\]). The first arguments to `get_x` functions
are always:

- `aoi`: an area of interest, provided as simple feature (see
  `sf::sf()`)
- `save_dir`: a directory to save the results to. The default (`NULL`)
  leads to the file `here::here("out", "ds", "x.rds")` being created and
  used as `save_file`. `ds` is for ‘data source’. While the saved file
  is usually `x.rds`, in some instances it follows the format and naming
  of the download from `x` (e.g. GBIF data comes in a `.zip` file named
  by the corresponding download key)
- `get_new`: an override to force `get_x` to requery the data source,
  even if save_file already exists
- `...`: the dots are passed to any underlying ‘native’ function, such
  as `rgbif::occ_download()`, `galah::galah_call()` or
  `ausplotsR::get_ausplots()`

Only the `get_x` functions for publicly available data are available
within envImport.

Within `get_x` functions the following steps are taken:

- query the data source for data within the area of interest
  - possibly including other criteria to limit the results
- adjust the results, if required, to better integrate with other data
  sources
- rename the columns to a common standard (via the data_map)
  - possibly adding some commonly desired fields at this step: `occ` (is
    this a presence \[`1`\] or absence \[`0`\] record?), `month` and
    `year`
- save the results, usually as .parquet

`get_x` functions can be run from `get_data`.

### Combine

No specific functions are provided for combining data. The following are
possible (assuming ‘files’ is a vector of file names resulting from
`get_x`):

- `purrr::map_dfr(files, \(x) rio::import(x, setclass = "tibble")`
- `arrow::open_dataset(files, unify_schema = TRUE) %>% dplyr::collect()`

`rio::import` is possibly more robust to differences in schema when
importing files (based on observation - needs testing).

# Cleaning

`envImport` ***does not clean*** data. Any combined dataset is likely to
contain all sorts of duplication and other spurious records. For help
cleaning data, see, for example:

- The book [Cleaning Biodiversity Data in
  R](https://cleaning-data-r.ala.org.au/)
- R packages:
  - [CoordinateCleaner](https://cran.r-project.org/web/packages/CoordinateCleaner/vignettes/Cleaning_GBIF_data_with_CoordinateCleaner.html)
  - [envClean](https://dew-landscapes.github.io/envClean/)
