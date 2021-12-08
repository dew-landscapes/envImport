#' Flag sensitive records according to BDBSA
#'
#' This will flag records from both taxa and surveys identified as sensitive by
#' the Biological Databases of South Australia
#' ([BDBSA](https://www.environment.sa.gov.au/topics/Science/Information_data/Biological_databases_of_South_Australia)).
#' Survey matches will only be done by `SURVEYNR` (although it can be named
#' something else, as specified in `surv_nr`). Likewise, taxa matches will only
#' be done by `NSXCODE` (which can also be named something else, as specified in
#' `nsx_col`).
#'
#' @param recs_df Dataframe of taxa records with `taxa_col` and `surv_col`.
#' @param nsx_col Character. Name of column containing the [taxa](https://data.environment.sa.gov.au/Content/Publications/DEW_SAEnvironmentallySensitiveDataREGISTER.xls)
#' `NSXCODE` to check for sensitivity.
#' @param surv_nr Character. Name of column containing BDBSA
#' [SURVEYNR](https://apps.environment.sa.gov.au/emap/envmaps-query.do?cmd=su.SurveySummaryMain)
#' to check for sensitivity. Must match a column name in both `recs_df` and
#' [SURVEYNR](https://apps.environment.sa.gov.au/emap/envmaps-query.do?cmd=su.SurveySummaryMain)
#'
#'
#' @return Dataframe with extra column(s) `sens_surv` and `sens_taxa` (logical).
#' @export
#'
#' @examples
flag_sens_records <- function(recs_df
                              , nsx_col = NULL
                              , surv_col = NULL
                              ) {

  if(isTRUE(!is.null(surv_col))) {

    # Get sensitive survey numbers
    sens_survey <- htmltab::htmltab(doc = "https://apps.environment.sa.gov.au/emap/envmaps-query.do?cmd=su.SurveySummaryMain"
                                    , which = 1
                                    ) %>%
      tibble::as_tibble() %>%
      dplyr::select(!!rlang::ensym(surv_col) := 1
                    , sens_surv = 5
                    ) %>%
      dplyr::mutate(sens_surv = grepl("Sensitive|sensitive", sens_surv)
                    , !!rlang::ensym(surv_col) := as.numeric(!!rlang::ensym(surv_col))
                    )

    recs_df <- recs_df %>%
      dplyr::left_join(sens_survey)

  }

  if(isTRUE(!is.null(nsx_col))) {

    # Get sensitive taxa
    sens_taxa <- rio::import("https://data.environment.sa.gov.au/Content/Publications/DEW_SAEnvironmentallySensitiveDataREGISTER.xls") %>%
      dplyr::select(!!rlang::ensym(nsx_col) := grep("NSXCODE", names(.), value = TRUE)) %>%
      dplyr::mutate(sens_taxa = TRUE)

    # combine sensitive taxa
    recs_df <- recs_df %>%
      dplyr::left_join(sens_taxa)

    }

  return(recs_df)

}
