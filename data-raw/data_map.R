
  data_map <- tibble::tibble(
    data_name = c("ALIS", "BCM", "BDBSA", "EGIS", "NVB", "PTP", "TERN", "GBIF"),
    order = c(3L, 5L, 1L, 2L, 7L, 6L, 4L, 8L),
    days = c(60, 60, 21, 21, 1000, 365, 60, 90),
    site = c("SITENUMBER", "SITE_ID", "PATCHID", "FLORACODE", "path", "PlantDataID", "site_unique", "gbifID"),
    date = c("SurveyDate", "ASSESSMENT_DATE", "VISITDATE", "SIGHTINGDATE", "date", "Obs_Date", "visit_start_date", "eventDate"),
    lat = c("LATITUDE", "LATITUDE", "LATITUDE", "LATITUDE", "lat", "LATITUDE", "latitude", "decimalLatitude"),
    long = c("LONGITUDE", "LONGITUDE", "LONGITUDE", "LONGITUDE", "lon", "LONGITUDE", "longitude", "decimalLongitude"),
    original_name = c("SPECIES", "SPECIES", "SPECIES", "SPECIES", "Spp", "SPECIES", "herbarium_determination", "species"),
    nsx = c("NSXCODE", "NSXCODE", "NSXCODE", "NSXCODE", NA, "NSXCODE", NA, "organismID"),
    #lifeform = c("Lifeform", NA, "MUIRCODE", NA, NA, "Life_form", "MUIRCODE", NA),
    #cover = c("Cover", NA, "COVER", NA, NA, NA, "COVER", "organismQuantity"),
    #cover_code = c(NA, NA, "COVCODE", NA, NA, "COVCODE", NA, NA),
    #lifespan = c("LIFESPAN", NA, "LIFESPAN", NA, NA, NA, NA, NA),
    survey_nr = c(NA, NA, "SURVEYNR", "SURVEYNR", NA, NA, NA, NA),
    survey = c(NA, NA, "SURVEYNAME", "SURVEYNAME", NA, NA, NA, NA),
    ind = c("ISINDIGENOUS", NA, "ISINDIGENOUS", "ISINDIGENOUSFLAG", NA, "Native_Introduced_original", NA, NA),
    rel_nr = c(NA, NA, "RELIABNR", "RELIABNR", NA, NA, NA, NA),
    rel_metres = c(NA, NA, NA, NA, NA, NA, NA, "coordinateUncertaintyInMeters"),
    #quad_x = c(NA, "X_DIM", "VEGQUADSIZE1", NA, NA, NA, "quadX", NA),
    #quad_y = c(NA, "Y_DIM", "VEGQUADSIZE2", NA, NA, NA, "quadY", NA),
    #site_desc = c(NA, "VEGETATION_DESCRIPTION", "OVUNDEMERGSTRING", "HABITATCOMM", NA, NA, NA, NA)
    desc = c("Arid lands information systems"
             , "Bushland condition monitoring"
             , "Biological databases of South Australia"
             , "Flora 'Supertable' from the environmental databases of South Australia"
             , "DEW Native Vegetation Branch"
             , "Paddock tree project"
             , "Terrestrial ecosystem network"
             , "Global biodiversity information facility"
             )
    ) %>%
    dplyr::mutate(data_name = forcats::fct_reorder(data_name, order)) %>%
    dplyr::arrange(order)
