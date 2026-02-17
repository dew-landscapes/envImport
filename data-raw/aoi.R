
  aoi <- envFunc::make_aoi(polygons = sfarrow::st_read_parquet(fs::path(envFunc::get_env_dir(), "data", "vector", "parks.parquet"))
                           , filt_col = "RESNAME"
                           , filt_level = "Cooltong"
                           , buffer = 10000
                           , bbox = TRUE
                           )
