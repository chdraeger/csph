library(sf)
library(raster)
library(exactextractr)
library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grafify)


# Shapefile
shp <- read_sf("~/Data/csph/shape_files/Madagascar_healthsheds2022/healthsheds2022.shp")
shp <- shp[!st_is_empty(shp), ]

# Calculate the population by healthshed
res <- c()
for(year in 2015:2024){
  
  cat(year, "\n")
  
  # r <- raster(paste0("~/Data/csph/population/globpop/GlobPOP_Count_30arc_", year, "_I32.tiff" )) # in longlat
  # r <- raster(paste0("~/Data/csph/population/landscan/landscan-global-", year, ".tif" )) # in longlat
  r <- raster(paste0("~/Data/csph/population/worldpop/mdg_pop_", year, "_CN_1km_R2025A_UA_v1.tif" )) # in longlat
  
  # check crs
  # st_crs(shp)$proj4string == crs(r)@projargs
  
  # extract shape
  shed <- exact_extract(r, shp, "sum", append_cols = c("fs_uid"))
  res <- rbind(
    res,
    cbind(shed, year = year)
  )
  
}

res_wide <- res |>
  pivot_wider(
    names_from = year,
    values_from = sum,
    names_prefix = "worldpop1km_" # landscan_", "globpop_", "worldpop100m_"
  )

# write.csv(res_wide, "~/Data/csph/population/shed_globpop.csv", row.names = FALSE)
# write.csv(res_wide, "~/Data/csph/population/shed_landscan.csv", row.names = FALSE)
# write.csv(res_wide, "~/Data/csph/population/shed_worldpop100m.csv", row.names = FALSE)
write.csv(res_wide, "~/Data/csph/population/shed_worldpop1km.csv", row.names = FALSE)


# # GHS-Pop
# #===============================================
# pop <- read_sf(paste0("~/Data/csph/population/ghspop/", year, "/GHSL2_0_MWD_L1_tile_schema_land.shp" )) # in longlat
# pop <- st_transform(pop, crs = 4326)



# Get data frame from all population sources
#===========================================================================================

g <- read.csv("~/Data/csph/population/shed_globpop.csv")
l <- read.csv("~/Data/csph/population/shed_landscan.csv")
w1 <- read.csv("~/Data/csph/population/shed_worldpop100m.csv")
w2 <- read.csv("~/Data/csph/population/shed_worldpop1km.csv")


clean_pop_shed <- function(df){
  
  df_long <- df %>%
    pivot_longer(
      cols = matches(".*_\\d{4}$"),   # any name ending with _YYYY (4 digits)
      names_to = c("dataset", "year"),
      names_sep = "_",
      values_to = "value"
    ) %>%
    mutate(year = as.integer(year))
  
  df_long
  
}


res <- rbind(clean_pop_shed(g),
             clean_pop_shed(l),
             clean_pop_shed(w1),
             clean_pop_shed(w2))

# Add official government-issued population counts
gov_2022 <- shp[, c("fs_uid", "fs_pop")]
gov_2022$geometry <- NULL
gov_2022 <- cbind(fs_uid = gov_2022$fs_uid, dataset="official", year = 2022, value = gov_2022$fs_pop)

gov <- read.csv("~/Data/csph/clinic/sectorisation/sec_2023_2024.csv")
gov <- gov %>%
  group_by(uid) %>%
  summarise(
    Pop_2023 = sum(Pop_2023, na.rm = TRUE),
    Pop_2024 = sum(Pop_2024, na.rm = TRUE),
    .groups = "drop"
  )
gov <- gov[3:nrow(gov),]  # not entirely correct. need to check for all

gov <- gov %>%
  rename(fs_uid = uid) %>%
  pivot_longer(
    cols = starts_with("Pop_"),
    names_to = "year",
    names_prefix = "Pop_",
    values_to = "value"
  ) %>%
  mutate(
    dataset = "official",
    year = as.integer(year)
  ) %>%
  select(fs_uid, dataset, year, value)

res <- rbind(res, gov_2022, gov)
res$year <- as.numeric(res$year)
res$value <- as.numeric(res$value)

res$dataset <- factor(res$dataset, levels = c("official", "globpop", "landscan", "worldpop100m", "worldpop1km"),
                      labels = c("Official", "GlobPop", "LandScan", "WorldPop 100m", "WorldPop 1km"))

# plot
#===========================================================================================

cols <- c("black", as.character(graf_palettes$kelly[1:4]))
  
# Toalagnaro Bazaribe
ggplot(res[res$fs_uid == "ANyiQnCJ1JJ",]) +
  geom_line(aes(x = year, y = value, col = dataset)) +
  ylab("Population") + xlab("Year") +
  labs(color = "Dataset", title = "Taolagnaro–Bazaribe") +
  theme_bw() +
  scale_color_manual(values = cols)

# Amparafaravola
ggplot(res[res$fs_uid == "As7NgbiFb4U",]) +
  geom_line(aes(x = year, y = value, col = dataset)) +
  ylab("Population") + xlab("Year") +
  labs(color = "Dataset", title = "Amparafaravola") +
  theme_bw() +
  scale_color_manual(values = cols)

# Anjozorobe
ggplot(res[res$fs_uid == "dZETRPZhGJP",]) +
  geom_line(aes(x = year, y = value, col = dataset)) +
  ylab("Population") + xlab("Year") +
  labs(color = "Dataset", title = "Anjozorobe") +
  theme_bw() +
  scale_color_manual(values = cols)


# get smoothed percentage changes
#================================================================================================
res_change <- res %>%
  group_by(fs_uid, dataset) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    change = (value - lag(value)) / lag(value)
  ) %>%
  filter(!is.na(change)) %>%
  ungroup()

# mean(res_change[res_change$dataset == "globpop",]$change)


a <- res_change[res_change$fs_uid == "As7NgbiFb4U" & res_change$dataset == "worldpop1km",]
sm <- loess(change ~ year, data = a, span = 5)

# plot(a$year, a$change * 100, type = "l", col = "grey60", lwd = 2, main = c("Amparafaravola"))
# lines(a$year, predict(sm), col = "red", lwd = 3)
# 
# sp <- smooth.spline(a$year, a$change, spar = 0.6)
# lines(sp, col = "blue", lwd = 3)
# predict(sp, a$year)$y


ggplot(a, aes(x = year, y = change * 100)) +
  geom_line(aes(color = "Raw"), linewidth = 1) +
  geom_smooth(aes(color = "Smoothed"), method = "loess", span =5, se = FALSE, linewidth = 1.2) +
  scale_color_manual(
    name = "Type",
    values = c("Raw" = "grey60", "Smoothed" = "red")
  ) +
  labs(
    title = "Amparafaravola",
    x = "Year",
    y = "Change (%)"
  ) +
  theme_bw() +
  scale_x_continuous(
    breaks = seq(2010, 2024, by = 2)
  ) +
  ylim(-5, 5)







# Only use positive values
a_sel <- a
a_sel <- a_sel[a_sel$change > 0, ]  # only use positive values
a_sel <- a_sel[a_sel$change < 0.05, ]  # cap at 5%

# Smooth
sp <- smooth.spline(a_sel$year, a_sel$change, spar = 0.6)
sm <- loess(a_sel$change ~ a_sel$year, span = 5) 

pred <- predict(sp, 2010:2024)$y

# Plot
lines(a_pos$year, pred, col = "orange", lwd = 3)


# Bias corrected counts
#==========================================================

res <- res[res$dataset %in% c("worldpop100m", "official"),]
res <- res[res$year == 2022,]

res <- res %>%
  pivot_wider(names_from = dataset, values_from = value) %>%
  mutate(factor = worldpop100m / official)

write.csv(res, "~/Data/csph/population/shed_worldpop100m_bias.csv", row.names = FALSE)


# Plot mean healthshed growth rate
#==========================================================
agg <- res_change %>% group_by(fs_uid, dataset) %>%
  summarize(change = mean(change))
agg <- agg[agg$dataset == "worldpop100m",]
agg$change <- agg$change * 100

shp_new <- left_join(shp, agg, by = c("fs_uid"))


# Plot the shapefile
ggplot(shp_new) +
  geom_sf(aes(fill = change), color = NA) +  # NA removes polygon borders
  scale_fill_viridis_c(option = "plasma", name = "Growth rate (%)") +  # Nice color scale
  theme_minimal() +
  labs(title = "Mean population growth in WorldPop (2015-2024)") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())


# Get bias-corrected time series
#===========================================================

bias <- read.csv("~/Data/csph/population/shed_worldpop100m_bias.csv")

final <- res[res$dataset == "worldpop100m",]
final <- left_join(final, bias[, c(1,5)], by = c("fs_uid"))
final$pop_corr <- final$value / final$factor 
final$factor <- final$value <- NULL
final$dataset <- "worldpop100m_corrected"
colnames(final)[4] <- "value"

agg <- rbind(res, final)




# plot
#===========================================================================================
agg$dataset <- factor(agg$dataset, levels = c("official", "globpop", "landscan", "worldpop100m", "worldpop100m_corrected", "worldpop1km"),
                      labels = c("Official", "GlobPop", "LandScan", "WorldPop 100m", "WorldPop 100m bias-corrected", "WorldPop 1km"))

cols <- c("black", as.character(graf_palettes$kelly[1:5]))


ggplot(agg[agg$fs_uid == "ANyiQnCJ1JJ",]) +
  geom_line(aes(x = year, y = value, col = dataset)) +
  ylab("Population") + xlab("Year") +
  labs(color = "Dataset", title = "Taolagnaro–Bazaribe") +
  theme_bw() +
  scale_color_manual(values = cols)


ggplot(agg[agg$fs_uid == "dZETRPZhGJP",]) +
  geom_line(aes(x = year, y = value, col = dataset)) +
  ylab("Population") + xlab("Year") +
  labs(color = "Dataset", title = "Anjozorobe") +
  theme_bw() +
  scale_color_manual(values = cols)


ggplot(agg[agg$fs_uid == "As7NgbiFb4U",]) +
  geom_line(aes(x = year, y = value, col = dataset)) +
  ylab("Population") + xlab("Year") +
  labs(color = "Dataset", title = "Amparafaravola") +
  theme_bw() +
  scale_color_manual(values = cols)


