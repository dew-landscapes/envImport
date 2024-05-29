
  library(magrittr)

  data_map <- tibble::tibble(
    data_name = c("alis", "bcm", "bdbsa", "egis", "nvb", "ptp", "tern", "gbif", "other"),
    epsg = c(4326, 4326, 7844, 7844, 4326, 4326, 4326, 4326, 4326),
    order = c(5L, 6L, 2L, 3L, 8L, 7L, 4L, 9L, 1L),
    days = c(60, 60, 700, 700, 1000, 700, 60, 90, 1000),
    site = c("SITENUMBER", "SITE_ID", "PATCHID", "EGISCODE", "path", "PlantDataID", "site_unique", "gbifID", "Site"),
    date = c("SurveyDate", "ASSESSMENT_DATE", "OBSDATE", "SIGHTINGDATE", "date", "Obs_Date", "visit_start_date", "eventDate", "SIGHTINGDATE"),
    lat = c("LATITUDE", "LATITUDE", "LATITUDE", "LATITUDE", "lat", "LATITUDE", "latitude", "decimalLatitude", "LATITUDE"),
    long = c("LONGITUDE", "LONGITUDE", "LONGITUDE", "LONGITUDE", "lon", "LONGITUDE", "longitude", "decimalLongitude", "LONGITUDE"),
    original_name = c("LegacyName", "Species", "SPECIES", "SPECIES", "Spp", "Scientific_name_original", "species", "species", "SPECIES"),
    common = c(NA, "Common1", "COMNAME1", "COMNAME", NA, "Common_name_orig", NA, NA, NA),
    nsx = c("NSXCode", "Old_NSX_Code", "NSXCODE", "NSXCODE", NA, "NSXCODE", NA, "organismID", NA),
    occ_derivation = c(NA, NA, "NUMOBSERVED", "NUMOBSERVED", NA, NA, NA, "occurrenceStatus", "NUMOBSERVED"),
    quantity = c(NA, NA, "NUMOBSERVED", "NUMOBSERVED", NA, NA, NA, "organismQuantity", "NUMOBSERVED"),
    survey_nr = c(NA, NA, "SURVEYNR", "SURVEYNR", NA, NA, NA, NA, "SURVEYNR"),
    survey = c("LandSystem", NA, "SURVEYNAME", "SURVEYNAME", NA, NA, NA, NA, "SURVEYNAME"),
    ind = c(NA, "isIndigenous", "ISINDIGENOUS", "ISINDIGENOUSFLAG", NA, "Native_Introduced_original", NA, NA, NA),
    rel_metres = c(NA, NA, "rel_metres", "rel_metres", NA, NA, NA, "coordinateUncertaintyInMeters", "maxDist"),
    sens = c(NA, NA, NA, "DISTRIBNDESC", NA, NA, NA, NA, NA),
    lifeform = c("Lifeform", NA, "MUIRCODE", NA, NA, "Life_form", "lifeform", NA, NA),
    lifespan = c(NA, "LIFESPAN", "LIFESPAN", NA, NA, NA, NA, NA, NA),
    cover = c("Cover", NA, "COVER", NA, NA, NA, "cover", "organismQuantity", NA),
    cover_code = c(NA, NA, "COVCODE", NA, NA, "Cover_abundance", NA, NA, NA),
    height = c(NA, NA, NA, NA, NA, NA, "height", NA, NA),
    quad_x = c(NA, "X_DIM", "VEGQUADSIZE1", NA, NA, NA, "quadX", NA, NA),
    quad_y = c(NA, "Y_DIM", "VEGQUADSIZE2", NA, NA, NA, "quadY", NA, NA),
    epbc_status = c(NA, NA, "ESACTSTATUSCODE", "ESACTSTATUSCODE", NA, NA, NA, NA, NA),
    npw_status = c(NA, NA, "NPWACTSTATUSCODE", "NPWACTSTATUSCODE", NA, NA, NA, NA, NA),
    method = c(NA, NA, "METHODDESC", "METHODDESC", NA, NA, NA, "samplingProtocol", "METHODDESC"),
    obs = c("observer", "assessor", "observer", "OBSERVER", "assessor", "Observers", "observer_veg", "recordedBy", "observer"),
    desc = c("Arid lands information systems"
             , "Bushland condition monitoring"
             , "Biological databases of South Australia"
             , "Occurrence datasets from the environmental databases of South Australia (e.g. supertables)"
             , "DEW Native Vegetation Branch"
             , "Paddock tree project"
             , "Terrestrial ecosystem network"
             , "Global biodiversity information facility"
             , "Other private datasets: SA Bird Atlas (UOA/Birds SA), Birdlife Australia Birdata portal, MLR Extra Bandicoot data, KI Post Fire Bird Monitoring, Cherry Gardens Post-fire Heath Bird Monitoring"
             )
    ) %>%
    dplyr::mutate(data_name = forcats::fct_reorder(data_name
                                                   , order
                                                   )
                  , data_name_use = toupper(data_name)
                  ) %>%
    dplyr::arrange(order)
