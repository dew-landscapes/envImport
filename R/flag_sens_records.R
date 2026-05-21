#' Flag sensitive records according to BDBSA
#'
#' This will flag records from both taxa and surveys identified as sensitive by
#' the Biological Databases of South Australia
#' ([BDBSA](https://www.environment.sa.gov.au/topics/Science/Information_data/Biological_databases_of_South_Australia)).
#' Survey matches will only be done by `Survey Number` (although it can be named
#' something else, as specified in `surv_nr`). Likewise, taxa matches will only
#' be done by `NSXCODE` (which can also be named something else, as specified in
#' `nsx_col`).
#'
#' @param recs_df Dataframe of taxa records with `nsx_col` and `surv_col`.
#' @param nsx_col Character. Name of column containing BDBSA `NSXCODE` to check
#' for
#' [sensitivity](https://data.environment.sa.gov.au/Content/Publications/DEW_SAEnvironmentallySensitiveDataREGISTER.xls).
#' @param surv_nr Character. Name of column containing BDBSA
#' `Survey Number` to check for
#' [sensitivity](https://apps.environment.sa.gov.au/emap/envmaps-query.do?cmd=su.SurveySummaryMain).
#'
#'
#' @return `recs_df` with extra column(s) `sens_surv` and `sens_taxa` (logical).
#' @export
#'
#' @examples
flag_sens_records <- function(recs_df
                              , nsx_col = NULL
                              , surv_col = NULL
                              ) {

  if(isTRUE(!is.null(surv_col))) {

    # Get sensitive survey numbers -------
    # connection
    con <- DBI::dbConnect(odbc::odbc()
                          , if(Sys.info()['sysname'] == "Windows") "BDBSA Production" else "BDBSA_Production"
                          , database = if(Sys.info()['sysname'] == "Windows") "BDBSA Production" else "EARX_PRD"
                          , uid = Sys.getenv("BDBSA_PRD_user")
                          , pwd = Sys.getenv("BDBSA_PRD_pwd")
                          )

    # From LM 2026/05/21
    # the distribution rules are found in the table SU.SUPERADMIN in field DATADISTRIBUTIONRULECODE.
    # To link that table to the SURVEYNR you will need to refer to the table SU.SUPERIOD and join the PERIODNR fields.
    # None of the surveys have multiple PERIODNR so you shouldn’t have to left or right join.
    admin <- dplyr::tbl(con,"SUPERADMIN")
    period <- dplyr::tbl(con, "SUPERIOD")

    lu_sens <- tibble::tribble(
      ~DATADISTRIBUTIONRULECODE, ~sens_surv,
      "1", "Public Dataset",
      "2", "Public Dataset",
      "3", "Sensitive Dataset: Data supplied to approved clients via DEWBioDataRequests@sa.gov.au",
      "4", "Sensitive Dataset: Written permission required from Information Authority"
    )

    sens_survey <- period |>
      dplyr::left_join(admin |>
                         dplyr::select(PERIODNR, DATADISTRIBUTIONRULECODE)
                       ) |>
      dplyr::distinct(SURVEYNR, DATADISTRIBUTIONRULECODE) |>
      dplyr::collect() |>
      dplyr::left_join(lu_sens)

    recs_df <- recs_df %>%
      dplyr::left_join(sens_survey)

  }

  if(isTRUE(!is.null(nsx_col))) {

    # Get sensitive taxa
    sens_taxa <- rio::import("https://data.environment.sa.gov.au/Content/Publications/DEW_SAEnvironmentallySensitiveDataREGISTER.xls"
                             , setclass = "tibble"
                             ) %>%
      dplyr::select(!!rlang::ensym(nsx_col) := grep("NSXCODE", names(.), value = TRUE)
                    , kingdom = Kingdom
                    , sens_taxa = Clarifier
                    )

    # combine sensitive taxa
    recs_df <- recs_df %>%
      dplyr::left_join(sens_taxa)

    }

  return(recs_df)

}
