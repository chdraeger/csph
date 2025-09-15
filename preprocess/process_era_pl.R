library(raster)
library(tidyr)

country <- "Madagascar"

pressures <- c("650","700","750","800","850","900","950","1000")

# read ERA5
temp <- raster::brick(paste0("~/Data/csph/ERA5/", country, "/orig/era5_pl_temperature_2010.grib"))  # every 6 hours
hgt <- raster::brick(paste0("~/Data/csph/ERA5/", country, "/orig/era5_pl_geopotential_2010.grib"))  

times <- getZ(temp)
# 4 hours * 365 days * 8 pressure levels = 11680

dup_layers <- duplicated(as.list(hgt))
hgt_unique <- dropLayer(hgt, which(dup_layers))


i = 1
while(i <= 11680){
  
  
  
}

gc()
temp_hour <- temp[[1:8]]
gc()
hgt <- hgt[[which(layers_hgt %in% layers_temp)]]

which(layers_hgt != layers_temp)
b <- duplicated(layers_hgt)

a <- getZ(hgt)
length(unique(a))


> head(split(dup_idx_all, layers_hgt[dup_idx_all]))
$`2010-11-01 06:00:00_X100`
[1]  9744 10013

test <- hgt[[9744]]
test[1:5, 1:5]

test2 <- hgt[[10013]]
test2[1:5, 1:5]

which(substr(names(temp), 1, 5) != substr(names(hgt), 1, 5))

# save average lapse rates as intermediary GeoTIFFs --> calculate with them


names(temp)[9990:10000]
names(hgt)[9990:10000]



# Convert extracted values into a data frame
df <- data.frame(
  time = times,
  pressure = rep(pressures, times = length(unique(times))),
  temperature = as.numeric(vals)
)


vals <- raster::extract(r, pt)
centr <- read.csv("~/Data/csph/shape_files/centroids_healthsheds_Madagascar.csv")
pt <- centr[2, c("lon", "lat")]