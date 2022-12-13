
#' Get occurrence records from BDBSA
#'
#' Requires an existing dsn "BDBSA Production" as well as BDBSA logon and
#' password.
#'
#' @param out_file Character. Path to save output data.
#' @param data_map Dataframe. Mapping of BDBSA fields to retrieve and their new
#' names
#' @param bdbsa_user Character
#' @param bdbsa_pwd Character
#' @param flora Logical. Return flora or fauna records.
#'
#' @return Dataframe and `rio::export(results, out_file)`
#' @export
#'
#' @examples
  get_BDBSA <- function(out_file
                        , data_map
                        , bdbsa_user = Sys.getenv("BDBSA_PRD_user")
                        , bdbsa_pwd = Sys.getenv("BDBSA_PRD_pwd")
                        , flora = TRUE
                        ) {

    # connect to BDBSA
    con <- dbConnect(odbc::odbc()
                     , "BDBSA Production"
                     , database = "BDBSA Production’"
                     , uid = bdbsa_user
                     , pwd = bdbsa_pwd
                     )

    # Link to each table
    excludeVars <- c("CREATED_DATE"
                     , "CREATED_USER"
                     , "MODIFIED_DATE"
                     , "MODIFIED_USER"
                     )

    # Survey
    sur <- dplyr::tbl(con,"LUSURVEYNAME") %>%
      dplyr::select(!tidyselect::any_of(excludeVars))

    # Patch
    pat <- dplyr::tbl(con,"SUPATCH") %>%
      dplyr::select(!tidyselect::any_of(excludeVars))

    # Visit
    vis <- dplyr::tbl(con,"SUVISIT") %>%
      dplyr::select(!tidyselect::any_of(excludeVars))

    # Species
    spp <- dplyr::tbl(con,"SUSPECIES") %>%
      dplyr::filter(if(flora) SPECIESTYPE == "P" else SPECIESTYPE != "P"
                    , DATEACCURACY != "C"
                    , DATEACCURACY != "T"
                    , ISCERTAIN == "Y"
                    , !NUMOBSERVED %in% c("0"
                                          , "none detected"
                                          , "None detected"
                                          )
                    ) %>%
      dplyr::select(!tidyselect::any_of(excludeVars))


    # lookup from 'not synonymous and not renamed' linked to to FL_FLSP

    nonsynnotren <- if(flora) "FLVNONSYNNOTREN" else "VSVNONSYNNOTREN"


    luFlor <- dplyr::tbl(con, nonsynnotren) %>%
      dplyr::left_join(dplyr::tbl(con,"FLSP") %>%
                         dplyr::select(SPECIESNR
                                       , NSXCODE
                                       , LIFESPAN
                                       , ISINDIGENOUS
                                       )
                       , by = c("SPECIESNR"
                                , "NSXCODE"
                                )
                       ) %>%
      dplyr::select(!tidyselect::any_of(excludeVars))


    # What names to grab before collect()?

    selectNames <- data_map %>%
      dplyr::filter(data_name == "BDBSA") %>%
      unlist(., use.names=FALSE) %>%
      stats::na.omit()


    # Get all records

    temp <- sur %>%
      dplyr::left_join(pat, by = "SURVEYNR") %>%
      dplyr::left_join(vis, by = "PATCHID") %>%
      dplyr::left_join(spp, by = "VISITNR") %>%
      dplyr::left_join(luFlor, by = "NSXCODE") %>%
      dplyr::collect() %>%
      dplyr::left_join(envImport::lurelBDBSA) %>%
      dplyr::select(tidyselect::any_of(selectNames)) %>%
      dplyr::filter(!is.na(SPECIES))

    dbDisconnect(con)


    # Export records

    rio::export(temp
                , out_file
                )


    # Return results

    return(temp)

  }

