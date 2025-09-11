library(sf)
library(raster)
library(exactextractr)

# shp <- read_sf("~/Data/csph/shape_files/mdg_healthsheds2022/healthsheds2022.shp")
shp <- read_sf("~/Data/csph/shape_files/Nepal_healthsheds2024/NepalLocalUnits0.shp")

# ext <-  extent(shp)
# ext <- extend(ext, c(1, 1))

shp_valid <- shp[!st_is_empty(shp), ]

# get center locations
# ce <- st_centroid(shp_valid)
# coords <- st_coordinates(ce)

# Madagascar
# centr <- data.frame(fs_uid  = ce$fs_uid, lon = coords[, "X"], lat = coords[, "Y"])
# write.csv(centr, "~/Data/csph/shape_files/centroids_healthsheds.csv", row.names = FALSE)

# # Nepal
# centr <- data.frame(fid  = ce$fid, lon = coords[, "X"], lat = coords[, "Y"])
# write.csv(centr, "~/Data/csph/shape_files/centroids_Nepal_healthsheds.csv", row.names = FALSE)

# # Fort-Dauphin
# fs_name <- "CSB2 Bazaribe Taolagnaro"
# 
# # Tana, at Lake Anosy
# fs_name <- "CSB2 Mahamasina"


res <- c()
for(ssp in c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")){
  
  for(year in seq(2020, 2100, by=5)){
    
    cat(ssp, year, "\n")
    
    r <- raster(paste0("~/Data/population_projection/data_raw/population_projection_1km/", ssp, "/", ssp, "_", year, ".tif" ))
    
    # extract shape in Madagascar
    shed <- exact_extract(r, shp_valid, "sum")
    res <- rbind(
      res,
      cbind("fid" = shp_valid$fid, ssp = ssp, year = year, population = shed)
    )
    
  }
  
}

# round
res <- as.data.frame(res)
res$population <- as.numeric(res$population)
res$population <- round(res$population, 2)

write.csv(res, "~/Data/csph/population/population_proj_healthsheds_Nepal.csv", row.names = FALSE)
