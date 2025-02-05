
  library(magrittr)

  if(FALSE) {

    # TEMPLATE------
    # replace "data_name_replace" with, say, "bdbsa"
    dplyr::left_join(
      tibble::tribble(
        ## data_name_replace ---------
        ~col, ~data_name_replace,
        "data_name", "data_name_replace",
        "epsg", "",
        "site", "",
        "date", "",
        "lat", "",
        "long", "",
        "original_name", "",
        "common", "",
        "nsx", "",
        "occ_derivation", "",
        "quantity", "",
        "survey_nr", "",
        "survey", "",
        "ind", "",
        "rel_metres", "",
        "lifeform", "",
        "lifespan", "",
        "cover", "",
        "cover_code", "",
        "height", "",
        "quad_metres", "",
        "epbc_status", "",
        "npw_status", "",
        "method", "",
        "obs", "",
        "denatured", "",
        "kingdom", "",
        "desc", "",
        "data_name_use", "",
        "url", ""
      )
    )

  }


  # set data names ------
  data_names = tibble::tibble(name = c("bdbsa",
                                       "egis",
                                       "thirdparty",
                                       "herbarium",
                                       "havplot",
                                       "tern",
                                       "abbbs",
                                       "alis",
                                       "bcm",
                                       "ptp",
                                       "nvb",
                                       "other",
                                       "galah",
                                       "gbif",
                                       "verdon"
                                       )
                              ) |>
    dplyr::mutate(value = as.character(dplyr::row_number())
                  , col = "order"
                  , class = "character"
                  , bio_all = FALSE
                  )

  data_map <- tibble::tribble(
    # data_map ---------
    ~col, ~class, ~bio_all,
    "data_name", "character", TRUE,
    "epsg", "numeric", FALSE,
    "site", "character", TRUE,
    "date", "Date", TRUE,
    "lat", "numeric", TRUE,
    "long", "numeric", TRUE,
    "original_name", "character", TRUE,
    "common", "character", TRUE,
    "nsx", "character", TRUE,
    "occ_derivation", "character", FALSE,
    "quantity", "character", TRUE,
    "survey_nr", "character", TRUE,
    "survey", "character", TRUE,
    "ind", "character", TRUE,
    "rel_metres", "character", TRUE,
    "lifeform", "character", TRUE,
    "lifespan", "character", TRUE,
    "cover", "numeric", TRUE,
    "cover_code", "character", TRUE,
    "height", "numeric", TRUE,
    "quad_metres", "numeric", TRUE,
    "epbc_status", "character", TRUE,
    "npw_status", "character", TRUE,
    "method", "character", TRUE,
    "obs", "character", TRUE,
    "denatured", "character", FALSE,
    "kingdom", "character", TRUE,
    "desc", "character", FALSE,
    "data_name_use", "character", FALSE,
    "url", "character", FALSE
    ) |>
    dplyr::left_join(
      tibble::tribble(
        ## bdbsa-------
        ~col, ~bdbsa,
        "data_name", "bdbsa",
        "epsg", "7844",
        "site", "PATCHID",
        "date", "OBSDATE",
        "lat", "LATITUDE",
        "long", "LONGITUDE",
        "original_name", "CONCATNAMAUTH",
        "common", "COMNAME1",
        "nsx", "NSXCODE",
        "occ_derivation", "NUMOBSERVED",
        "quantity", "NUMOBSERVED",
        "survey_nr", "SURVEYNR",
        "survey", "SURVEYNAME",
        "ind", "ISINDIGENOUS",
        "rel_metres", "rel_metres",
        "lifeform", "MUIRCODE",
        "lifespan", "LIFESPAN",
        "cover", "COVER",
        "cover_code", "COVCODE",
        "quad_metres", "quad_metres",
        "epbc_status", "ESACTSTATUSCODE",
        "npw_status", "NPWACTSTATUSCODE",
        "method", "METHODDESC",
        "obs", "observer",
        "kingdom", "kingdom",
        "desc", "Biological databases of South Australia",
        "data_name_use", "BDBSA",
        "url", "https://www.environment.sa.gov.au/topics/science/information-and-data/biological-databases-of-south-australia"
        )
      ) |>
    dplyr::left_join(
      tibble::tribble(
        ## egis ---------
        ~col, ~egis,
        "data_name", "egis",
        "epsg", "7844",
        "site", "EGISCODE",
        "date", "SIGHTINGDATE",
        "lat", "LATITUDE",
        "long", "LONGITUDE",
        "original_name", "SPECIES",
        "common", "COMNAME",
        "nsx", "NSXCODE",
        "occ_derivation", "NUMOBSERVED",
        "quantity", "NUMOBSERVED",
        "survey_nr", "SURVEYNR",
        "survey", "SURVEYNAME",
        "ind", "ISINDIGENOUSFLAG",
        "rel_metres", "rel_metres",
        "epbc_status", "ESACTSTATUSCODE",
        "npw_status", "NPWACTSTATUSCODE",
        "method", "METHODDESC",
        "obs", "OBSERVER",
        "kingdom", "kingdom",
        "desc", "Occurrence datasets from the environmental databases of South Australia (e.g. supertables)",
        "data_name_use", "EGIS",
        "url", "https://data.sa.gov.au/data/dataset/enviro-data-sa-website"
      )
    ) |>
    dplyr::left_join(
      tibble::tribble(
        ## thirdparty ---------
        ~col, ~thirdparty,
        "data_name", "thirdparty",
        "epsg", "7844",
        "site", "EGISCODE",
        "date", "SIGHTINGDATE",
        "lat", "LATITUDE",
        "long", "LONGITUDE",
        "original_name", "SPECIES",
        "common", "COMNAME",
        "nsx", "NSXCODE",
        "occ_derivation", "NUMOBSERVED",
        "quantity", "NUMOBSERVED",
        "survey_nr", "SURVEYNR",
        "survey", "SURVEYNAME",
        "ind", "ISINDIGENOUSFLAG",
        "rel_metres", "rel_metres",
        "epbc_status", "ESACTSTATUSCODE",
        "npw_status", "NPWACTSTATUSCODE",
        "method", "METHODDESC",
        "obs", "OBSERVER",
        "kingdom", "kingdom",
        "desc", "Third party occurrence datasets from the environmental databases of South Australia",
        "data_name_use", "thirdparty",
        "url", "https://data.sa.gov.au/data/dataset/enviro-data-sa-website"
      )
    ) |>
    dplyr::left_join(
      tibble::tribble(
        ## herbarium ---------
        ~col, ~herbarium,
        "data_name", "herbarium",
        "epsg", "7844",
        "site", "EGISCODE",
        "date", "SIGHTINGDATE",
        "lat", "LATITUDE",
        "long", "LONGITUDE",
        "original_name", "SPECIES",
        "common", "COMNAME",
        "nsx", "NSXCODE",
        "occ_derivation", "NUMOBSERVED",
        "quantity", "NUMOBSERVED",
        "survey_nr", "SURVEYNR",
        "survey", "SURVEYNAME",
        "ind", "ISINDIGENOUSFLAG",
        "rel_metres", "rel_metres",
        "epbc_status", "ESACTSTATUSCODE",
        "npw_status", "NPWACTSTATUSCODE",
        "method", "METHODDESC",
        "obs", "OBSERVER",
        "kingdom", "kingdom",
        "desc", "Herbarium occurrence dataset from the environmental databases of South Australia",
        "data_name_use", "Herbarium",
        "url", "https://data.sa.gov.au/data/dataset/enviro-data-sa-website"
      )
    ) |>
    dplyr::left_join(
      tibble::tribble(
        ## havplot ---------
        ~col, ~havplot,
        "data_name", "havplot",
        "epsg", "4326",
        "site", "plotName",
        "date", "obsStartDate",
        "lat", "decimalLatitude",
        "long", "decimalLongitude",
        "original_name", "scientificName",
        "occ_derivation", "abundanceValue",
        "quantity", "abundanceValue",
        "survey", "projectID",
        "rel_metres", "coordinateUncertaintyInMetres",
        "cover", "cover",
        "quad_metres", "quad_metres",
        "method", "abundanceMethod",
        "obs", "individualName",
        "kingdom", "kingdom",
        "desc", "Harmonised Australian Vegetation Plot dataset",
        "data_name_use", "HAVPlot",
        "url", "https://researchdata.edu.au/harmonised-australian-vegetation-dataset-havplot/1950860"
      )
    ) |>
    dplyr::left_join(
      tibble::tribble(
        ## abbbs -------
        ~col, ~abbbs,
        "data_name", "abbbs",
        "epsg", "4326",
        "site", "BANDING_LOCALITY",
        "date", "DATE_BANDED",
        "lat", "LATITUDE",
        "long", "LONGITUDE",
        "original_name", "SCIENA",
        "occ_derivation", "NUMOBSERVED",
        "survey_nr", "SURVEYNR",
        "common", "COMMON",
        "obs", "BANDER",
        "kingdom", "kingdom",
        "desc", "Australian Bird and Bat Banding Scheme",
        "data_name_use", "ABBBS",
        "url", "https://www.dcceew.gov.au/science-research/bird-bat-banding"
        )
      ) |>
    dplyr::left_join(
      tibble::tribble(
        ## tern ---------
        ~col, ~tern,
        "data_name", "tern",
        "epsg", "4326",
        "site", "site_unique",
        "date", "visit_start_date",
        "lat", "latitude",
        "long", "longitude",
        "original_name", "species",
        "lifeform", "lifeform",
        "cover", "cover",
        "height", "height",
        "quad_metres", "quad_metres",
        "obs", "observer_veg",
        "kingdom", "kingdom",
        "desc", "Terrestrial ecosystem network",
        "data_name_use", "TERN",
        "url", "https://www.tern.org.au/"
      )
    ) |>
    dplyr::left_join(
      tibble::tribble(
        ## alis ---------
        ~col, ~alis,
        "data_name", "alis",
        "epsg", "4326",
        "site", "SITENUMBER",
        "date", "SurveyDate",
        "lat", "LATITUDE",
        "long", "LONGITUDE",
        "original_name", "CONCATNAMAUTH",
        "common", "COMNAME1",
        "nsx", "NSXCODE",
        "survey_nr", "SURVEYNR",
        "survey", "LandSystem",
        "ind", "ISINDIGENOUS",
        "lifeform", "Lifeform",
        "lifespan", "LIFESPAN",
        "cover", "Cover",
        "epbc_status", "ESACTSTATUSCODE",
        "npw_status", "NPWACTSTATUSCODE",
        "obs", "observer",
        "kingdom", "kingdom",
        "desc", "Arid lands information system",
        "data_name_use", "ALIS",
        "url", "https://www.pir.sa.gov.au/aghistory/natural_resources/pastoral_land_management"
      )
    ) |>
    dplyr::left_join(
      tibble::tribble(
        ## nvb ---------
        ~col, ~nvb,
        "data_name", "nvb",
        "epsg", "4326",
        "site", "path",
        "date", "date",
        "lat", "lat",
        "long", "lon",
        "original_name", "Spp",
        "obs", "assessor",
        "kingdom", "kingdom",
        "desc", "DEW Native Vegetation Branch",
        "data_name_use", "NVB",
        "url", "https://www.environment.sa.gov.au/topics/native-vegetation"
      )
    ) |>
    dplyr::left_join(
      tibble::tribble(
        ## bcm ---------
        ~col, ~bcm,
        "data_name", "bcm",
        "epsg", "4326",
        "site", "SITE_ID",
        "date", "ASSESSMENT_DATE",
        "lat", "LATITUDE",
        "long", "LONGITUDE",
        "original_name", "CONCATNAMAUTH",
        "common", "COMNAME1",
        "nsx", "species",
        "ind", "ISINDIGENOUS",
        "lifespan", "LIFESPAN",
        "quad_metres", "quad_metres",
        "survey_nr", "SURVEYNR",
        "epbc_status", "ESACTSTATUSCODE",
        "npw_status", "NPWACTSTATUSCODE",
        "obs", "assessor",
        "kingdom", "kingdom",
        "desc", "Bushland condition monitoring",
        "data_name_use", "BCM",
        "url", "https://www.ncssa.asn.au/bushland-health-and-condition/"
      )
    ) |>
    dplyr::left_join(
      tibble::tribble(
        ## other ---------
        ~col, ~other,
        "data_name", "other",
        "epsg", "4326",
        "site", "Site",
        "date", "SIGHTINGDATE",
        "lat", "LATITUDE",
        "long", "LONGITUDE",
        "original_name", "SPECIES",
        "occ_derivation", "NUMOBSERVED",
        "quantity", "NUMOBSERVED",
        "survey_nr", "SURVEYNR",
        "survey", "SURVEYNAME",
        "rel_metres", "maxDist",
        "method", "METHODDESC",
        "obs", "observer",
        "kingdom", "kingdom",
        "desc", "Other private datasets: SA Bird Atlas (UOA/Birds SA), Birdlife Australia Birdata portal, MLR Extra Bandicoot data, KI Post Fire Bird Monitoring, SA Seed Conservation Centre",
        "data_name_use", "Other"
      )
    ) |>
    dplyr::left_join(
      tibble::tribble(
        ## ptp ---------
        ~col, ~ptp,
        "data_name", "ptp",
        "epsg", "4326",
        "site", "PlantDataID",
        "date", "Obs_Date",
        "lat", "LATITUDE",
        "long", "LONGITUDE",
        "original_name", "CONCATNAMAUTH",
        "common", "COMNAME1",
        "nsx", "NSXCODE",
        "survey_nr", "SURVEYNR",
        "ind", "ISINDIGENOUS",
        "lifeform", "Life_form",
        "lifespan", "LIFESPAN",
        "cover_code", "Cover_abundance",
        "epbc_status", "ESACTSTATUSCODE",
        "npw_status", "NPWACTSTATUSCODE",
        "obs", "Observers",
        "kingdom", "kingdom",
        "desc", "Paddock tree project",
        "data_name_use", "PTP",
        "url", "https://treesforlife.org.au/TFLWeb/TFLWeb/What_we_do/Projects/Paddock-Tree-Projects.aspx#:~:text=Mount%20Lofty%20Ranges%20Paddock%20Tree,whose%20numbers%20are%20in%20decline."
      )
    ) |>
    dplyr::left_join(
      tibble::tribble(
        ## galah ---------
        ~col, ~galah,
        "data_name", "galah",
        "epsg", "4326",
        "site", "locationID",
        "date", "eventDate",
        "lat", "decimalLatitude",
        "long", "decimalLongitude",
        "original_name", "scientificName",
        "common", "vernacularName",
        "nsx", "organismID",
        "occ_derivation", "occurrenceStatus",
        "quantity", "organismQuantity",
        "survey", "datasetName",
        "rel_metres", "coordinateUncertaintyInMeters",
        "method", "samplingProtocol",
        "obs", "recordedBy",
        "denatured", "generalisationInMetres",
        "kingdom", "kingdom",
        "desc", "Atlas of Living Australia",
        "data_name_use", "ALA",
        "url", "https://www.ala.org.au/"
      )
    ) |>
    dplyr::left_join(
      tibble::tribble(
        ## gbif ---------
        ~col, ~gbif,
        "data_name", "gbif",
        "epsg", "4326",
        "site", "gbifID",
        "date", "eventDate",
        "lat", "decimalLatitude",
        "long", "decimalLongitude",
        "original_name", "scientificName",
        "occ_derivation", "occurrenceStatus",
        "quantity", "organismQuantity",
        "rel_metres", "coordinateUncertaintyInMeters",
        "method", "samplingProtocol",
        "obs", "recordedBy",
        "denatured", "informationWithheld",
        "kingdom", "kingdom",
        "desc", "Global biodiversity information facility",
        "data_name_use", "GBIF",
        "url", "https://www.gbif.org/"
      )
    ) |>
    dplyr::left_join(
      tibble::tribble(
        ## verdon-------
        ~col, ~verdon,
        "data_name", "verdon",
        "epsg", "7844",
        "site", "site_id",
        "date", "survey_date",
        "lat", "POINT_Y",
        "long", "POINT_X",
        "original_name", "scientific_name",
        "occ_derivation", "nu_total",
        "quantity", "nu_total",
        "survey_nr", "survey_nr",
        "survey", "survey",
        "rel_metres", "rel_metres",
        "method", "method",
        "obs", "speciesobs1",
        "kingdom", "kingdom",
        "desc", "Presence records at 4 ha scale from Simon Verdon's heath mallee bird work",
        "data_name_use", "Verdon",
        "url", "https://opal.latrobe.edu.au/articles/dataset/Threatened_Mallee_Birds_in_Heathlands_Project_-_Data_for_N-mixture_models/25417102"
        )
      ) |>
    tidyr::pivot_longer(tidyselect::any_of(data_names$name)) |>
    dplyr::bind_rows(data_names) |>
    tidyr::pivot_wider()


  # data_map_old--------
  data_map_old <- tibble::tibble(
    data_name = c("havplot", "alis", "bcm", "bdbsa", "egis", "nvb", "ptp", "tern", "gbif", "galah", "other"),
    order = c(3, 5, 7, 1, 2, 5, 9, 4, 11, 10, 8),
    epsg = c(4326, 4326, 4326, 7844, 7844, 4326, 4326, 4326, 4326, 4326, 4326),
    site = c("plotName", "SITENUMBER", "SITE_ID", "PATCHID", "EGISCODE", "path", "PlantDataID", "site_unique", "gbifID", "locationID", "Site"),
    date = c("obsStartDate", "SurveyDate", "ASSESSMENT_DATE", "OBSDATE", "SIGHTINGDATE", "date", "Obs_Date", "visit_start_date", "eventDate", "eventDate", "SIGHTINGDATE"),
    lat = c("decimalLatitude", "LATITUDE", "LATITUDE", "LATITUDE", "LATITUDE", "lat", "LATITUDE", "latitude", "decimalLatitude", "decimalLatitude", "LATITUDE"),
    long = c("decimalLongitude", "LONGITUDE", "LONGITUDE", "LONGITUDE", "LONGITUDE", "lon", "LONGITUDE", "longitude", "decimalLongitude", "decimalLongitude", "LONGITUDE"),
    original_name = c("scientificName", "CONCATNAMAUTH", "CONCATNAMAUTH", "CONCATNAMAUTH", "SPECIES", "Spp", "CONCATNAMAUTH", "species", "scientificName", "scientificName", "SPECIES"),
    common = c(NA, "COMNAME1", "COMNAME1", "COMNAME1", "COMNAME", NA, "COMNAME1", NA, NA, "vernacularName", NA),
    nsx = c(NA, "NSXCode", "species", "NSXCODE", "NSXCODE", NA, "NSXCODE", NA, "organismID", "organismID", NA),
    occ_derivation = c("abundanceValue", NA, NA, "NUMOBSERVED", "NUMOBSERVED", NA, NA, NA, "occurrenceStatus", "occurrenceStatus", "NUMOBSERVED"),
    quantity = c("abundanceValue", NA, NA, "NUMOBSERVED", "NUMOBSERVED", NA, NA, NA, "organismQuantity", "organismQuantity", "NUMOBSERVED"),
    survey_nr = c(NA, NA, NA, "SURVEYNR", "SURVEYNR", NA, NA, NA, NA, NA, "SURVEYNR"),
    survey = c("projectID", "LandSystem", NA, "SURVEYNAME", "SURVEYNAME", NA, NA, NA, NA, "datasetName", "SURVEYNAME"),
    ind = c(NA, "ISINDIGENOUS", "ISINDIGENOUS", "ISINDIGENOUS", "ISINDIGENOUSFLAG", NA, "ISINDIGENOUS", NA, NA, NA, NA),
    rel_metres = c("coordinateUncertaintyInMetres", NA, NA, "rel_metres", "rel_metres", NA, NA, NA, "coordinateUncertaintyInMeters", "coordinateUncertaintyInMeters", "maxDist"),
    lifeform = c(NA, "Lifeform", NA, "MUIRCODE", NA, NA, "Life_form", "lifeform", NA, NA, NA),
    lifespan = c(NA, "LIFESPAN", "LIFESPAN", "LIFESPAN", NA, NA, "LIFESPAN", NA, NA, NA, NA),
    cover = c("cover", "Cover", NA, "COVER", NA, NA, NA, "cover", NA, NA, NA),
    cover_code = c(NA, NA, NA, "COVCODE", NA, NA, "Cover_abundance", NA, NA, NA, NA),
    height = c(NA, NA, NA, NA, NA, NA, NA, "height", NA, NA, NA),
    quad_x = c("length", NA, "X_DIM", "VEGQUADSIZE1", NA, NA, NA, "quadX", NA, NA, NA),
    quad_y = c("width", NA, "Y_DIM", "VEGQUADSIZE2", NA, NA, NA, "quadY", NA, NA, NA),
    quad_metres = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    epbc_status = c(NA, "ESACTSTATUSCODE", "ESACTSTATUSCODE", "ESACTSTATUSCODE", "ESACTSTATUSCODE", NA, NA, NA, NA, NA, NA),
    npw_status = c(NA, "NPWACTSTATUSCODE", "NPWACTSTATUSCODE", "NPWACTSTATUSCODE", "NPWACTSTATUSCODE", NA, NA, NA, NA, NA, NA),
    method = c("abundanceMethod", NA, NA, "METHODDESC", "METHODDESC", NA, NA, NA, "samplingProtocol", "samplingProtocol", "METHODDESC"),
    obs = c("individualName", "observer", "assessor", "observer", "OBSERVER", "assessor", "Observers", "observer_veg", "recordedBy", "recordedBy", "observer"),
    denatured = c(NA, NA, NA, NA, NA, NA, NA, NA, "informationWithheld", "generalisationInMetres", NA),
    desc = c("Harmonised Australian Vegetation Plot dataset (HAVPlot)"
             , "Arid lands information systems"
             , "Bushland condition monitoring"
             , "Biological databases of South Australia"
             , "Occurrence datasets from the environmental databases of South Australia (e.g. supertables)"
             , "DEW Native Vegetation Branch"
             , "Paddock tree project"
             , "Terrestrial ecosystem network"
             , "Global biodiversity information facility"
             , "Atlas of Living Australia"
             , "Other private datasets: SA Bird Atlas (UOA/Birds SA), Birdlife Australia Birdata portal, MLR Extra Bandicoot data, KI Post Fire Bird Monitoring, SA Seed Conservation Centre"
             )
    ) |>
    dplyr::mutate(kingdom = "kingdom"
                  , data_name = forcats::fct_reorder(data_name
                                                     , order
                                                     )
                  , data_name_use = dplyr::case_when(data_name == "havplot" ~ "HAVPlot",
                                                     data_name == "other" ~ "Other",
                                                     data_name == "galah" ~ "ALA",
                                                     TRUE ~ toupper(data_name)
                                                     )
                  ) |>
    dplyr::arrange(order)
