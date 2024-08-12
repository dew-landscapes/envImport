#' Simple feature to define a geographic area of interest.
#'
#' `aoi` defines an area in the South Australian riverland, centred on Cooltong
#' Conservation Park.
#'
#' @format A simple feature polygon
"aoi"

#' Lookup the maximum reliability distance for `RELIBANR` field
#'
#'
#' @format A data frame with `r nrow(lurelBDBSA)` rows and `r ncol(lurelBDBSA)`
#' variables:
#' \describe{
#'   \item{RELIANR}{As per LU_LURELIABNR in BDBSA}
#'   \item{max_dist}{Corresponding distance in metres}
#'   ...
#' }
"lurelBDBSA"

#' (Old version) Lookup for mapping data source columns to united column
#'
#'
#' @format A data frame with `r nrow(data_map_old)` rows and `r ncol(data_map_old)`
#' variables where column names provide the new name for columns in the original
#' data sources:
#' \describe{
#'   \item{data_name}{Short name of data source}
#'   \item{order}{Integer. Preferential order for using data if assessing
#'   duplication}
#'   \item{days}{How many days to allow before attempting to update? Rarely used
#'   anymore. More efficient to update manually via explicitly running `get_`
#'   and `make_`}
#'   \item{site}{}
#'   \item{date}{}
#'   \item{lat}{Latitude}
#'   \item{long}{Longitude}
#'   \item{original_name}{Name of column with taxa information}
#'   \item{nsx}{Name of column with taxa codes}
#'   \item{number}{Number observed}
#'   \item{surveynr}{Name of column with survey number (or code)}
#'   \item{survey}{Name of survey}
#'   \item{ind}{Flag for non-indigenous taxa}
#'   \item{rel_nr}{RELIABNR in `envClean::lurelBDBSA`}
#'   \item{rel_metres}{Spatial reliability in metres}
#'   \item{sens}{Sensitivity flag for EGIS data}
#'   \item{lifespan}{Muircode}
#'   \item{cover}{Cover value as proportion}
#'   \item{cover_code}{Cover value as modified Braun-Blanquet code}
#'   \item{quad_x}{Length of quadrat size in x direction}
#'   \item{quad_y}{Lenght of quadrat size in y direction}
#'   \item{desc}{Sentence description of `data_name`}
#'   ...
#' }
"data_map_old"

#' Lookup for mapping data source columns to united column
#'
#'
#' @format A data frame with `r nrow(data_map)` rows and `r ncol(data_map)`
#' variables where column names are `col` (new column names)
#' , `class` (class of new columns)
#' , `final_select_col` (usually `bio_all`) and an extra column for each
#' `data_name` mapping `data_name` column names to `col` column names
#' data sources:
#' \describe{
#'   \item{col}{Character. Name of column in new data frame}
#'   \item{class}{Character. What class will the new column be?}
#'   \item{`final_select_col`}{Logical. Will this column end up in the united
#'   data frame (or, the alternative, is it needed from the original data source
#'   for processing)}
#'   \item{`data_name`}{Any number of columns named for each data source and
#'   populated with the old column names in that data source}
#'   ...
#' }
"data_map"
