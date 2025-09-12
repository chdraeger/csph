library(raster)
library(tidyr)
library(sf)
library(exactextractr)
library(terra)
library(dplyr)
library(data.table)


#####
country <- "Nepal"
id_name <- "fid"  # fid, fs_uid
year <- 2024
variable <- "2m_temperature"

pop <- rast("/home/christina/Data/csph/population/npl_pop_2020_CN_100m_R2025A_v1.tif")   ##### CHANGE

# shp <- read_sf("~/Data/csph/shape_files/Madagascar_healthsheds2022/healthsheds2022.shp")
shp <- read_sf("~/Data/csph/shape_files/Nepal_healthsheds2024/NepalLocalUnits0.shp")
#####

shp_valid <- shp[!st_is_empty(shp), ]

# read ERA5
file <- paste0("~/Data/csph/ERA5/", country, "/era5_sl_", variable, "_", year, ".grib")

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
            min = min(value, na.rm = TRUE)
            )

agg <- as.data.frame(agg)
agg$mean <- round(agg$mean, 2)
agg$min <- round(agg$min, 2)
agg$max <- round(agg$max, 2)

write.csv(agg, paste0("/home/christina/Data/csph/ERA5/", country, "/era5_sl_", variable, "_", year, "_pop_weighted.csv"),
          row.names = FALSE)
