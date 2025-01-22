
  aoi <- envFunc::make_aoi(polygons = sfarrow::st_read_parquet(fs::path("H:", "data", "vector", "parks.parquet"))
                           , filt_col = "RESNAME"
                           , filt_level = "Cooltong"
                           , buffer = 10000
                           , bbox = TRUE
                           )
