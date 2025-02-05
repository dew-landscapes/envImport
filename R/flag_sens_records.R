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

    # Get sensitive survey numbers
    sens_survey <- rvest::read_html(x = "https://apps.environment.sa.gov.au/emap/envmaps-query.do?cmd=su.SurveySummaryMain") %>%
      rvest::html_table() %>%
      `[[`(1) %>%
      tibble::as_tibble() %>%
      dplyr::select(!!rlang::ensym(surv_col) := 1
                    , sens_surv = 5
                    )

    recs_df <- recs_df %>%
      dplyr::left_join(sens_survey)

  }

  if(isTRUE(!is.null(nsx_col))) {

    # Get sensitive taxa
    sens_taxa <- rio::import("https://data.environment.sa.gov.au/Content/Publications/DEW_SAEnvironmentallySensitiveDataREGISTER.xls") %>%
      dplyr::select(!!rlang::ensym(nsx_col) := grep("NSXCODE", names(.), value = TRUE)
                    , sens_taxa = Clarifier
                    )

    # combine sensitive taxa
    recs_df <- recs_df %>%
      dplyr::left_join(sens_taxa)

    }

  return(recs_df)

}
