library(raster)
library(tidyr)
library(sf)
library(exactextractr)
library(terra)
library(dplyr)
library(data.table)

options(scipen=999)

# Configuration
#=========================================================================================================================================
country <- "Madagascar"  # Madagascar, Nepal
id_name <- "fs_uid"  # fid for Nepal, fs_uid for Madagascar
variable <- "surface_solar_radiation_downwards"
round_digits <- 2  # 4 for precipitation, volumetric_soil_water, 2 for others
pop_dataset <- "worldpop"  # worldpop, landscan

# Variables:  # total_precipitation, volumetric_soil_water_layer_1, surface_solar_radiation_downwards, 2m_dewpoint_temperature, 2m_temperature
#=========================================================================================================================================

for(year in 2010:2024){
  
  # Read data
  #====================================
  
  # Read population
  pop_year <- ifelse(year < 2015, 2015, year)  #### CHANGE based on population data set
  pop <- rast(paste0("/home/christina/Data/csph/population/", pop_dataset, "/mdg_pop_", pop_year, "_CN_100m_R2025A_v1.tif"))   ##### CHANGE; Worldpop
  # pop <- rast(paste0("/home/christina/Data/csph/population/", pop_dataset, "/landscan-global-2023.tif"))   ##### CHANGE; Landscan
  pop[is.na(pop)] <- 0  # Worldpop data: replace NANs with 0 (no population)
  
  # Read healthshed shapes
  shp <- read_sf("~/Data/csph/shape_files/Madagascar_healthsheds2022/healthsheds2022.shp")   ##### CHANGE
  # shp <- read_sf("~/Data/csph/shape_files/Nepal_healthsheds2024/NepalLocalUnits0.shp")
  shp_valid <- shp[!st_is_empty(shp), ]
  
  # Read ERA5
  file <- paste0("~/Data/csph/ERA5/", country, "/orig/era5_sl_", variable, "_", year, ".grib")
  r <- rast(file)
  times <- time(r)
  r <- project(r, crs(shp_valid))  # Reproject to shapefile CRS
  
  # Read elevation
  if(variable == "2m_temperature"){
    elevation <- read.csv(paste0("~/Data/csph/elevation/elevation_shed_", country, ".csv"))
    elevation <- elevation[, c(id_name, "elevation_srtm_pop", "elevation_era_pop")]
  }
  
  # Preprocess
  #====================================
  
  # Resample population raster (100 m) to match ERA5 raster (30 km)
  pop_resampled <- terra::resample(pop, r, method = "bilinear")
  pop_resampled[is.na(pop_resampled)] <- 0
  
  # Extract data per shape
  #====================================
  
  # Extract variable per shape weighted by population
  names(r) <- times
  agg <- exact_extract(r, shp_valid, "weighted_mean", weights = pop_resampled, append_cols = id_name)  # matrix: sheds x times
  colnames(agg) <- c(id_name, as.character(times))
  
  # Reshape to long format
  agg <- reshape2::melt(agg, id = c(id_name), variable.name = c("date_time"))
  
  # Postprocess
  #====================================
  
  # Change units
  if(variable == "surface_solar_radiation_downwards"){
    agg$value <- agg$value / 3600  # from Jm-2 to Wm-2
  }
  
  # Add temperature lapse rate correction
  # Current version: Take global mean lapse rate of 6.5 degC / 1000m
  if(variable == "2m_temperature"){
    agg <- left_join(agg, elevation, by = id_name)
    agg$value <- agg$value - (agg$elevation_srtm_pop - agg$elevation_era_pop) * 6.5 / 1000
  }
  
  # Extract day
  agg$day <- substr(agg$date_time, 1, 10)
  
  # Summarize by day
  agg <- agg %>%
    group_by(across(all_of(id_name)), day) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              max = max(value, na.rm = TRUE),
              min = min(value, na.rm = TRUE)
              # sum = sum(value, na.rm = TRUE)
    )
  
  # Fill gaps
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
  
  # Round
  agg <- as.data.frame(agg)
  agg$mean <- round(agg$mean, round_digits)
  agg$min <- round(agg$min, round_digits)
  agg$max <- round(agg$max, round_digits)
  # agg$sum <- round(agg$sum, round_digits)
  
  # Save
  #====================================
  write.csv(agg,
            paste0("/home/christina/Data/csph/ERA5/", country, "/era5_sl_", variable, "_", year, "_pop_weighted_", pop_dataset, ".csv"),
            row.names = FALSE)
  
}
