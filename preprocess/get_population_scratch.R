# Current population
#=================================================================================
library(dplyr)
library(reshape2)
library(ggplot2)

# Shapefile
shp <- read_sf("~/Data/csph/shape_files/Madagascar_healthsheds2022/healthsheds2022.shp")
shp <- shp[!st_is_empty(shp), ]

# read population
pop_w <- rast(paste0("/home/christina/Data/csph/population/worldpop/mdg_pop_2022_CN_100m_R2025A_v1.tif"))   ##### CHANGE    Worldpop
pop_w2 <- rast(paste0("/home/christina/Data/csph/population/worldpop/mdg_pop_2022_CN_1km_R2025A_UA_v1.tif"))   ##### CHANGE    Worldpop
pop_l <- rast(paste0("/home/christina/Data/csph/population/landscan/landscan-global-2022.tif"))   ##### CHANGE   Landscan


landscan <- exact_extract(pop_l, shp, "sum", append_cols = c("fs_uid")) # matrix: sheds x times
worldpop <- exact_extract(pop_w, shp, "sum", append_cols = c("fs_uid")) # matrix: sheds x times
worldpop2 <- exact_extract(pop_w2, shp, "sum", append_cols = c("fs_uid")) # matrix: sheds x times

df <- left_join(landscan, worldpop, by = c("fs_uid"))
df <- left_join(df, worldpop2, by = c("fs_uid"))
colnames(df) <- c("fs_uid", "landscan", "worldpop100m", "worldpop1km")

df <- left_join(df, shp[, c("fs_uid", "fs_pop")])
df$geometry <- NULL

# add Dimeji's landscan data
dim <- read.csv("~/Data/csph/population/dimeji_landscan_population_2000_2023.csv")
dim_wide <- dim |>
  pivot_wider(
    names_from = year,
    values_from = landscan_pop
  )
dim_wide <- dim_wide[, c("fs_uid", "2022")]

df <- left_join(df, dim_wide, by = "fs_uid")
colnames(df)[6] <- "dimeji"

# differences
df$diff_worldpop100m <- abs(df$worldpop100m - df$fs_pop)
df$diff_worldpop1km <- abs(df$worldpop1km - df$fs_pop)
df$diff_landscan <- abs(df$landscan - df$fs_pop)

diff <- melt(df[, c("fs_uid", "diff_worldpop100m", "diff_worldpop1km", "diff_landscan")],
             by = c("fs_uid"))

# mean(diff[diff$variable == "diff_worldpop100m",]$value)
# sum(df$worldpop1km)
