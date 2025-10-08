library(sf)
library(raster)
library(exactextractr)
library(terra)
library(dplyr)
library(tidyr)

lu <- raster("/media/christina/NEWEXT/WORLDCOVER/ESA_WORLDCOVER_10M_2021_V200/MAP/merged.tif")
shp <- read_sf("~/Data/csph/shape_files/Madagascar_healthsheds2022/healthsheds2022.shp")
shp_valid <- shp[!st_is_empty(shp), ]

shed <- exact_extract(lu, shp_valid, "frac")
shed$fs_uid <- shp_valid$fs_uid


wc <- read.csv("~/Data/csph/landuse/worldcover_shed.csv")
rowSums(wc[,2:10])
wc$frac_70 <- 0
wc$frac_100 <- 0
write.csv(wc, "~/Data/csph/landuse/worldcover_shed.csv")
