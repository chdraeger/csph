require(raster)
library(sf)
library(raster)
library(exactextractr)
library(terra)
library(dplyr)

# Process SRTM files
#####################################################
# files <- list.files("/home/christina/Data/csph/SRMT1/Nepal", full.names = TRUE)
# 
# for(i in 1:length(files)){
#   
#   cat(i)
#   r <- rast(files[i])
#   rnew <- r
#   na_count <- as.numeric(global(rnew, fun = function(x) sum(is.na(x))))
#   
#   if(na_count == 0){
#     cat("no NAs\n")
#   }else{
#     while(na_count > 0){
#       rnew <- terra::focal(rnew, fun=mean, na.policy="only", na.rm=T) # other option: terra::interpIDW
#       na_count <- as.numeric(global(rnew, fun = function(x) sum(is.na(x))))
#     }
#     cat("interpolated \n")
#   }
#   new_file <- sub("(\\.tif)$", "_new\\1", files[i])
#   terra::writeRaster(rnew, filename=new_file, overwrite = TRUE)
#   
# }
# 
# # command line: gdalwarp *_new.tif merged.tif

# (1) Get elevation per healthshed - ERA5 CENTROID
##############################################################################################################

centr <- read.csv("~/Data/csph/shape_files/centroids_healthsheds_Madagascar.csv")
# centr <- read.csv("~/Data/csph/shape_files/centroids_healthsheds_Nepal.csv")

# read ERA5
file <- paste0("~/Data/csph/ERA5/Madagascar/era5_sl.grib")
r <- raster::brick(file)

pts <- centr[, c("lon", "lat")]

elev <- raster::extract(r, pts)

# Convert extracted values into a data frame
df <- data.frame(
  fs_uid = centr$fs_uid,  #  fid = centr$fid,  
  elevation_era = round(as.numeric(elev) / 9.80665, 2)
)

df_ordered <- df[order(df$fs_uid), ]

write.csv(df_ordered, "~/Data/csph/elevation/elevation_shed_era_centroid_Madagascar.csv", row.names = FALSE)


# (2-3) Get elevation per healthshed - ERA5 (method currently used by the database + correct method)
##############################################################################################################
# Tinashe: We extract all the data points across the region, and then do temporal aggregation (hourly to daily) for each point,
# then do spatial aggregation by overlaying the healthshed onto the data points and assigning each point to the respective healthshed
# and taking the mean of the temporal aggregation

# currently used: centroids of ERA5 cells are just assigned to a healthshed. correct: area-averaged


# (2) ERA5- area-averaged (correct)
#######################
shp <- read_sf("~/Data/csph/shape_files/Madagascar_healthsheds2022/healthsheds2022.shp")
# shp <- read_sf("~/Data/csph/shape_files/Nepal_healthsheds2024/NepalLocalUnits0.shp")

shp_valid <- shp[!st_is_empty(shp), ]

# read ERA5
file <- paste0("~/Data/csph/ERA5/Madagascar/era5_sl_geopotential.grib")
r <- raster::brick(file)
r <- projectRaster(r, crs = crs(shp_valid))

# correct
shed <- exact_extract(r, shp_valid, "mean")

# Convert extracted values into a data frame
df <- data.frame(
  fs_uid = shp_valid$fs_uid,  #  fid fs_uid
  elevation_area_avg_corr = round(as.numeric(shed) / 9.80665, 2)
)

# df_ordered <- df[order(df$fid), ]
df_ordered <- df

write.csv(df_ordered, "~/Data/csph/elevation/elevation_shed_era_area_avg_corr_Madagascar.csv", row.names = FALSE)


# (3) currently used (not included)
########################

r_terra <- rast(r)
r_pts <- as.points(r_terra)
r_sf <- st_as_sf(r_pts)
colnames(r_sf)[1] <- "era_centroid"
joined <- st_join(r_sf, shp_valid, join = st_within)

means <- joined %>%
  group_by(fs_uid) %>%  # replace with your polygon ID column
  summarise(mean_val = mean(era_centroid, na.rm = TRUE))
means$mean_val <- means$mean_val / 9.81

# only 688 values for Madagascar: because some centroids don't fall into any health shed...

# # Convert extracted values into a data frame
# df <- data.frame(
#   fs_uid = shp_valid$fs_uid,  #  fid fs_uid
#   elevation_area_avg_corr = round(as.numeric(shed) / 9.80665, 2)
# )
# 
# # df_ordered <- df[order(df$fid), ]
# df_ordered <- df
# 
# write.csv(df_ordered, "~/Data/csph/elevation/elevation_shed_era_area_avg_curr_Madagascar.csv", row.names = FALSE)



# (4) ERA5: population-averaged
#######################
# shp <- read_sf("~/Data/csph/shape_files/Madagascar_healthsheds2022/healthsheds2022.shp")
shp <- read_sf("~/Data/csph/shape_files/Nepal_healthsheds2024/NepalLocalUnits0.shp")

# pop <- rast("/home/christina/Data/csph/population/mdg_pop_2020_CN_100m_R2025A_v1.tif")
pop <- rast("/home/christina/Data/csph/population/npl_pop_2020_CN_100m_R2025A_v1.tif")

shp_valid <- shp[!st_is_empty(shp), ]

# read ERA5
file <- paste0("~/Data/csph/ERA5/Nepal/era5_sl_geopotential.grib")
r <- rast(file)
r <- project(r, crs(shp_valid))

# population
pop[is.na(pop)] <- 0

# Resample population raster to match elevation raster
pop_resampled <- terra::resample(pop, r, method = "bilinear")  # much faster than raster package
pop_resampled[is.na(pop_resampled)] <- 0  # Worldpop data: replace NANs with 0 (no population)

# extract shape
shed <- exact_extract(r, shp_valid, "weighted_mean", weights = pop_resampled)

# Convert extracted values into a data frame
df <- data.frame(
  fid = shp_valid$fid,  #  fid fs_uid
  elevation_era_pop_weighted = round(as.numeric(shed) / 9.80665, 2)
)

df_ordered <- df[order(df$fid), ]
# df_ordered <- df

write.csv(df_ordered, "~/Data/csph/elevation/elevation_shed_era_pop_weighted_Nepal.csv", row.names = FALSE)


# (5) Get elevation per healthshed - SRTM mean
##############################################################################################################

shp <- read_sf("~/Data/csph/shape_files/Madagascar_healthsheds2022/healthsheds2022.shp")
# shp <- read_sf("~/Data/csph/shape_files/Nepal_healthsheds2024/NepalLocalUnits0.shp")

shp_valid <- shp[!st_is_empty(shp), ]

r <- raster("/home/christina/Data/csph/SRMT1/Madagascar/merged.tif")

# extract shape
shed <- exact_extract(r, shp_valid, "mean")

df <- data.frame(
  fs_uid = shp_valid$fs_uid,
  elevation_srtm = round(shed, 2)
)
df_ordered <- df[order(df$fs_uid), ]

write.csv(df_ordered, "~/Data/csph/elevation/elevation_shed_srtm_Madagascar.csv", row.names = FALSE)


# (6) Get elevation - SRTM WEIGHTED BY POPULATION per healthshed
##############################################################################################################

# shp <- read_sf("~/Data/csph/shape_files/Madagascar_healthsheds2022/healthsheds2022.shp")
shp <- read_sf("~/Data/csph/shape_files/Nepal_healthsheds2024/NepalLocalUnits0.shp")

shp_valid <- shp[!st_is_empty(shp), ]

r <- rast("/home/christina/Data/csph/elevation/SRMT1/Nepal/merged.tif")
pop <- rast("/home/christina/Data/csph/population/npl_pop_2020_CN_100m_R2025A_v1.tif")

# Resample population raster to match elevation raster
r_resampled <- terra::resample(r, pop, method = "bilinear")  # much faster than raster package

# Worldpop data: replace NANs with 0 (no population)
pop[is.na(pop)] <- 0

# extract shape
shed <- exact_extract(r_resampled, shp_valid, "weighted_mean", weights = pop)

df <- data.frame(
  fid = shp_valid$fid,  # fs_uid = shp_valid$fs_uid,  
  elevation_pop = round(shed, 2)
)
# df_ordered <- df[order(df$fid), ]
df_ordered <- df

write.csv(df_ordered, "~/Data/csph/elevation/elevation_shed_pop_Nepal.csv", row.names = FALSE)




###### merge into 1 file
country <- "Nepal"

era <- read.csv(paste0("~/Data/csph/elevation/elevation_shed_era_", country, ".csv"))
pop <- read.csv(paste0("~/Data/csph/elevation/elevation_shed_pop_", country, ".csv"))
srtm <- read.csv(paste0("~/Data/csph/elevation/elevation_shed_srtm_", country, ".csv"))

all <- left_join(era, srtm, by = c("fid"))  # fs_uid
all <- left_join(all, pop, by = c("fid"))

write.csv(all, paste0("~/Data/csph/elevation/elevation_shed_", country, ".csv"), row.names = FALSE)

###
all <- read.csv(paste0("~/Data/csph/elevation/elevation_shed_", country, ".csv"))
era_mean <- read.csv(paste0("~/Data/csph/elevation/elevation_shed_era_area_avg_corr_", country, ".csv"))

all <- left_join(all, era_mean, by = c("fs_uid"))
write.csv(all, paste0("~/Data/csph/elevation/elevation_shed_", country, ".csv"), row.names = FALSE)

########
all <- read.csv(paste0("~/Data/csph/elevation/elevation_shed_", country, ".csv"))
new <- read.csv(paste0("~/Data/csph/elevation/elevation_shed_era_pop_weighted_", country, ".csv"))

all <- left_join(all, new, by = c("fid"))
write.csv(all, paste0("~/Data/csph/elevation/elevation_shed_", country, ".csv"))
