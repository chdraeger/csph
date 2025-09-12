library(raster)
library(tidyr)

country <- "Madagascar"

centr <- read.csv("~/Data/csph/shape_files/centroids_healthsheds_Madagascar.csv")
pressures <- c("650","700","750","800","850","900","950","1000")

# read ERA5
file <- paste0("~/Data/csph/ERA5/pl_", country, "/era5_pl_2008.grib")
r <- raster::brick(file)

pt <- centr[2, c("lon", "lat")]
vals <- raster::extract(r, pt)

times <- getZ(r)

# Convert extracted values into a data frame
df <- data.frame(
  time = times,
  pressure = rep(pressures, times = length(unique(times))),
  temperature = as.numeric(vals)
)

