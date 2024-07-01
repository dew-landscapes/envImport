
  library(magrittr)

  data_map <- tibble::tibble(
    data_name = c("havplot", "alis", "bcm", "bdbsa", "egis", "nvb", "ptp", "tern", "gbif", "other"),
    order = c(3, 5, 7, 1, 2, 5, 9, 4, 10, 8),
    epsg = c(4326, 4326, 4326, 7844, 7844, 4326, 4326, 4326, 4326, 4326),
    site = c("plotName", "SITENUMBER", "SITE_ID", "PATCHID", "EGISCODE", "path", "PlantDataID", "site_unique", "gbifID", "Site"),
    date = c("obsStartDate", "SurveyDate", "ASSESSMENT_DATE", "OBSDATE", "SIGHTINGDATE", "date", "Obs_Date", "visit_start_date", "eventDate", "SIGHTINGDATE"),
    lat = c("decimalLatitude", "LATITUDE", "LATITUDE", "LATITUDE", "LATITUDE", "lat", "LATITUDE", "latitude", "decimalLatitude", "LATITUDE"),
    long = c("decimalLongitude", "LONGITUDE", "LONGITUDE", "LONGITUDE", "LONGITUDE", "lon", "LONGITUDE", "longitude", "decimalLongitude", "LONGITUDE"),
    original_name = c("scientificName", "LegacyName", "Species", "SPECIES", "SPECIES", "Spp", "Scientific_name_original", "species", "species", "SPECIES"),
    common = c(NA, NA, "Common1", "COMNAME1", "COMNAME", NA, "Common_name_orig", NA, NA, NA),
    nsx = c(NA, "NSXCode", "Old_NSX_Code", "NSXCODE", "NSXCODE", NA, "NSXCODE", NA, "organismID", NA),
    occ_derivation = c("abundanceValue", NA, NA, "NUMOBSERVED", "NUMOBSERVED", NA, NA, NA, "occurrenceStatus", "NUMOBSERVED"),
    quantity = c("abundanceValue", NA, NA, "NUMOBSERVED", "NUMOBSERVED", NA, NA, NA, "organismQuantity", "NUMOBSERVED"),
    survey_nr = c(NA, NA, NA, "SURVEYNR", "SURVEYNR", NA, NA, NA, NA, "SURVEYNR"),
    survey = c("projectID", "LandSystem", NA, "SURVEYNAME", "SURVEYNAME", NA, NA, NA, NA, "SURVEYNAME"),
    ind = c(NA, NA, "isIndigenous", "ISINDIGENOUS", "ISINDIGENOUSFLAG", NA, "Native_Introduced_original", NA, NA, NA),
    rel_metres = c("coordinateUncertaintyInMetres", NA, NA, "rel_metres", "rel_metres", NA, NA, NA, "coordinateUncertaintyInMeters", "maxDist"),
    sens = c(NA, NA, NA, NA, "DISTRIBNDESC", NA, NA, NA, NA, NA),
    lifeform = c(NA, "Lifeform", NA, "MUIRCODE", NA, NA, "Life_form", "lifeform", NA, NA),
    lifespan = c(NA, NA, "LIFESPAN", "LIFESPAN", NA, NA, NA, NA, NA, NA),
    cover = c("cover", "Cover", NA, "COVER", NA, NA, NA, "cover", "organismQuantity", NA),
    cover_code = c(NA, NA, NA, "COVCODE", NA, NA, "Cover_abundance", NA, NA, NA),
    height = c(NA, NA, NA, NA, NA, NA, NA, "height", NA, NA),
    quad_x = c("length", NA, "X_DIM", "VEGQUADSIZE1", NA, NA, NA, "quadX", NA, NA),
    quad_y = c("width", NA, "Y_DIM", "VEGQUADSIZE2", NA, NA, NA, "quadY", NA, NA),
    epbc_status = c(NA, NA, NA, "ESACTSTATUSCODE", "ESACTSTATUSCODE", NA, NA, NA, NA, NA),
    npw_status = c(NA, NA, NA, "NPWACTSTATUSCODE", "NPWACTSTATUSCODE", NA, NA, NA, NA, NA),
    method = c("abundanceMethod", NA, NA, "METHODDESC", "METHODDESC", NA, NA, NA, "samplingProtocol", "METHODDESC"),
    obs = c("individualName", "observer", "assessor", "observer", "OBSERVER", "assessor", "Observers", "observer_veg", "recordedBy", "observer"),
    denatured = c(NA, NA, NA, NA, NA, NA, NA, NA, "informationWithheld", NA),
    desc = c("Harmonised Australian Vegetation Plot dataset (HAVPlot)"
             , "Arid lands information systems"
             , "Bushland condition monitoring"
             , "Biological databases of South Australia"
             , "Occurrence datasets from the environmental databases of South Australia (e.g. supertables)"
             , "DEW Native Vegetation Branch"
             , "Paddock tree project"
             , "Terrestrial ecosystem network"
             , "Global biodiversity information facility"
             , "Other private datasets: SA Bird Atlas (UOA/Birds SA), Birdlife Australia Birdata portal, MLR Extra Bandicoot data, KI Post Fire Bird Monitoring, SA Seed Conservation Centre"
             )
    ) %>%
    dplyr::mutate(data_name = forcats::fct_reorder(data_name
                                                   , order
                                                   )
                  , data_name_use = dplyr::case_when(data_name == "havplot" ~ "HAVPlot",
                                                     data_name == "other" ~ "Other",
                                                     TRUE ~ toupper(data_name)
                                                     )
                  ) %>%
    dplyr::arrange(order)
