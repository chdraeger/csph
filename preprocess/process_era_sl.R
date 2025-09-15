library(raster)
library(tidyr)
library(sf)
library(exactextractr)
library(terra)
library(dplyr)
library(data.table)

#########################
country <- "Nepal"
id_name <- "fid"  # fid, fs_uid
year <- 2024
variable <- "total_precipitation"  # total_precipitation, volumetric_soil_water_layer_1
round_digits <- 4

# read population
pop <- rast(paste0("/home/christina/Data/csph/population/npl_pop_", year, "_CN_100m_R2025A_v1.tif"))   ##### CHANGE

# healthshed shapes
# shp <- read_sf("~/Data/csph/shape_files/Madagascar_healthsheds2022/healthsheds2022.shp")   ##### CHANGE
shp <- read_sf("~/Data/csph/shape_files/Nepal_healthsheds2024/NepalLocalUnits0.shp")

#########################



options(scipen=999)
shp_valid <- shp[!st_is_empty(shp), ]

# read ERA5
file <- paste0("~/Data/csph/ERA5/", country, "/orig/era5_sl_", variable, "_", year, ".grib")

r <- rast(file)
times <- time(r)
r <- project(r, crs(shp_valid))  # Reproject to shapefile CRS

# population
pop[is.na(pop)] <- 0  # Worldpop data: replace NANs with 0 (no population)

# Resample population raster (100 m) to match ERA5 raster (30 km)
pop_resampled <- terra::resample(pop, r, method = "bilinear")  # much faster than raster package
pop_resampled[is.na(pop_resampled)] <- 0

# extract shape weithed by population
names(r) <- times
shed <- exact_extract(r, shp_valid, "weighted_mean", weights = pop_resampled) # matrix: sheds x times

# aggregate
agg <- shed
colnames(agg) <- times
agg[,id_name] <- shp_valid[[id_name]]

# Melt to long format
agg <- reshape2::melt(agg, id = c(id_name))

agg$day <- substr(agg$variable, 1, 10)
agg$variable <- NULL

# summarize
agg <- agg %>%
  group_by(across(all_of(id_name)), day) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            sum = sum(value, na.rm = TRUE)
            )

# fill gaps
if(country == "Nepal"){
  # fid 734 is at the border and has only missing values. Take values from neighbor
  # centr <- read.csv("/home/christina/Data/csph/shape_files/centroids_healthsheds_Nepal.csv")
  # target <- centr[centr$fid == 734, ]
  # distances <- sqrt((centr$lon - target$lon)^2 + (centr$lat - target$lat)^2)
  # distances[centr$fid == 734] <- NA
  # closest_idx <- which.min(distances)
  # closest_point <- centr[closest_idx, ]  # 731

  # For each day of fid 734, find the corresponding day in 731
  for(d in agg$day[agg$fid == 734]){
    # get values from fid 731 for this day
    agg[agg$fid == 734 & agg$day == d, c("mean", "min", "max", "sum")] <-
      agg[agg$fid == 731 & agg$day == d, c("mean", "min", "max", "sum")]
  }
  
}

# aggregate
agg <- as.data.frame(agg)
agg$mean <- round(agg$mean, round_digits)
agg$min <- round(agg$min, round_digits)
agg$max <- round(agg$max, round_digits)
agg$sum <- round(agg$sum, round_digits)

# save
write.csv(agg, paste0("/home/christina/Data/csph/ERA5/", country, "/era5_sl_", variable, "_", year, "_pop_weighted.csv"),
          row.names = FALSE)
