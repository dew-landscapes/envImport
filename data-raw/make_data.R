
  rm(list = ls())

  library(magrittr)

  codes <- grep("make_data"
                , list.files(path = "data-raw",pattern = "\\.R$", full.names = TRUE)
                , value = TRUE
                , invert = TRUE
                )

  lapply(codes,source)

  datas <- ls(pattern = "lu|data_map")

  do.call(save, c(lapply(datas,as.name), file = "data/data.rda"))


