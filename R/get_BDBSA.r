
#' Get occurrence records from BDBSA
#'
#' Requires an existing dsn "BDBSA Production" as well as BDBSA logon and
#' password.
#'
#' @param save_file Character. File path into which to save outputs. If `null`
#' results will be saved to `fs::path("out", "ds", "name", "name_raw.rds")`
#' where `name` is the data source name.
#' @param get_new Logical. If FALSE, will attempt to load from existing
#' @param bdbsa_user Character
#' @param bdbsa_pwd Character
#' @param flora Logical. Return flora or fauna records.
#'
#' @return Dataframe and `rio::export(results, save_file)`
#' @export
#'
#' @examples
  get_BDBSA <- function(save_file = NULL
                        , get_new = FALSE
                        , bdbsa_user = Sys.getenv("BDBSA_PRD_user")
                        , bdbsa_pwd = Sys.getenv("BDBSA_PRD_pwd")
                        , flora = TRUE
                        ) {

    name <- "BDBSA"

    if(is.null(save_file)) {

      save_file <- fs::path("out"
                            , "ds"
                            , name
                            , paste0(name
                                     , "_"
                                     , if(flora) "flora" else "fauna"
                                     , "_raw.rds"
                                     )
                            )

    }

    # run query
    get_new <- if(!file.exists(save_file)) TRUE else get_new

    if(get_new) {

      fs::dir_create(dirname(save_file))

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


      # Get all records

      temp <- sur %>%
        dplyr::left_join(pat, by = "SURVEYNR") %>%
        dplyr::left_join(vis, by = "PATCHID") %>%
        dplyr::left_join(spp, by = "VISITNR") %>%
        dplyr::left_join(luFlor, by = "NSXCODE") %>%
        dplyr::collect()


      dbDisconnect(con)


      # Export records

      rio::export(temp
                  , save_file
                  )

    } else {

      temp <- rio::import(save_file)

    }

    return(temp)

  }

