
  data_map <- tibble::tibble(
    data_name = c("ALIS", "BCM", "BDBSA", "EGIS", "NVB", "PTP", "TERN", "GBIF", "BA"),
    order = c(4L, 5L, 1L, 2L, 7L, 6L, 3L, 8L, 9L),
    days = c(60, 60, 700, 700, 1000, 700, 60, 90, 365),
    site = c("SITENUMBER", "SITE_ID", "PATCHID", "EGISCODE", "path", "PlantDataID", "site_unique", "gbifID", "PATCHID"),
    date = c("SurveyDate", "ASSESSMENT_DATE", "VISITDATE", "SIGHTINGDATE", "date", "Obs_Date", "visit_start_date", "eventDate", "SIGHTINGDATE"),
    lat = c("LATITUDE", "LATITUDE", "LATITUDE", "LATITUDE", "lat", "LATITUDE", "latitude", "decimalLatitude", "LATITUDE"),
    long = c("LONGITUDE", "LONGITUDE", "LONGITUDE", "LONGITUDE", "lon", "LONGITUDE", "longitude", "decimalLongitude", "LONGITUDE"),
    original_name = c("LegacyName", "Species", "SPECIES", "SPECIES", "Spp", "Scientific_name_original", "herbarium_determination", "species", "SPECIES as supplied"),
    nsx = c("NSXCode", "Old_NSX_Code", "NSXCODE", "NSXCODE", NA, "NSXCODE", NA, "organismID", "NSXCODE"),
    number = c(NA, NA, "NUMOBSERVED","NUMOBSERVED", NA, NA, NA, NA, "NUMOBSERVED"),
    survey_nr = c(NA, NA, "SURVEYNR", "SURVEYNR", NA, NA, NA, NA, "SURVEYNR"),
    survey = c(NA, NA, "SURVEYNAME", "SURVEYNAME", NA, NA, NA, NA, "SURVEYNAME"),
    ind = c(NA, "isIndigenous", "ISINDIGENOUSFLAG", "ISINDIGENOUSFLAG", NA, "Native_Introduced_original", NA, NA, NA),
    rel_nr = c(NA, NA, "RELIABNR", "RELIABNR", NA, NA, NA, NA, "RELIABNR"),
    rel_metres = c(NA, NA, NA, NA, NA, NA, NA, "coordinateUncertaintyInMeters", NA),
    sens = c(NA, NA, NA, "DISTRIBNDESC", NA, NA, NA, NA, NA),
    desc = c("Arid lands information systems"
             , "Bushland condition monitoring"
             , "Biological databases of South Australia"
             , "Flora 'Supertable' from the environmental databases of South Australia"
             , "DEW Native Vegetation Branch"
             , "Paddock tree project"
             , "Terrestrial ecosystem network"
             , "Global biodiversity information facility"
             , "BirdLife Australia"
             )
    ) %>%
    dplyr::mutate(data_name = forcats::fct_reorder(data_name, order)
                  , rel_metres = dplyr::case_when(rel_nr == "RELIABNR" ~ "rel_metres"
                                                  , TRUE ~ rel_metres
                                                  )
                  ) %>%
    dplyr::arrange(order)
