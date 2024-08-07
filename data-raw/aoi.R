
  aoi <- envFunc::make_aoi(layer = sfarrow::st_read_parquet(fs::path("H:", "data", "vector", "parks.parquet"))
                           , filt_col = "RESNAME"
                           , level = "Cooltong"
                           , buffer = 10000
                           , bbox = TRUE
                           )
