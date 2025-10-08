library(sf)
library(raster)
library(exactextractr)
library(terra)
library(dplyr)
library(readxl)
library(tidyr)

shp <- read_sf("~/Data/csph/shape_files/Madagascar_healthsheds2022/healthsheds2022.shp")
# shp <- read_sf("~/Data/csph/shape_files/Nepal_healthsheds2024/NepalLocalUnits0.shp")

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


# Projections
#=================================================================================
res <- c()
for(ssp in c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")){
  
  for(year in seq(2020, 2100, by=5)){
    
    cat(ssp, year, "\n")
    
    r <- raster(paste0("~/Data/population_projection/data_raw/population_projection_1km/", ssp, "/", ssp, "_", year, ".tif" ))
    
    # extract shape
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



# Current population
#=================================================================================
# read population
pop_w <- rast(paste0("/home/christina/Data/csph/population/worldpop/mdg_pop_2020_CN_100m_R2025A_v1.tif"))   ##### CHANGE    Worldpop
pop_l <- rast(paste0("/home/christina/Data/csph/population/landscan/landscan-global-2023.tif"))   ##### CHANGE   Landscan


landscan <- exact_extract(pop_l, shp_valid, "sum", append_cols = c("fs_uid")) # matrix: sheds x times
worldpop <- exact_extract(pop_w, shp_valid, "sum", append_cols = c("fs_uid")) # matrix: sheds x times

df <- left_join(landscan, worldpop, by = c("fs_uid"))
colnames(df) <- c("fs_uid", "landscan", "worldpop")

df <- left_join(df, shp[, c("fs_uid", "fs_pop")])
df$geometry <- NULL


dim <- read.csv("~/Downloads/landscan_population_2000_2023.csv")
dim_wide <- dim |>
  pivot_wider(
    names_from = year,
    values_from = landscan_pop
  )
dim_wide <- dim_wide[, c("fs_uid", "2023")]

df <- left_join(df, dim_wide, by = "fs_uid")
colnames(df)[5] <- "dimeji"

# 
# sec <- read.csv("~/Downloads/sec.csv")
# a <- sec[, c("fs_uid")]
# 
# 
# write.csv(shed, paste0("/home/christina/Data/csph/population/pop_", pop_dataset, ".csv"))
# sec <- read_csv("~/Downloads/sec.csv", locale = locale(encoding = "UTF-8"))


library(readr)
sec <- read_csv("~/Downloads/sec.csv", locale = locale(encoding = "windows-1252-8"))

