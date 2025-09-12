require(reshape2)
require(dplyr)
require(ggplot2)

country <- "Nepal"
id_name <- "fid"  # fid


df <- read.csv(paste0("~/Data/csph/elevation/elevation_shed_", country, ".csv"))
df$elevation_era_centroid <- NULL

df <- melt(df, id = c(id_name))

df$variable <- factor(df$variable,
                      levels = c("elevation_era_area_avg", "elevation_srtm", "elevation_pop"),
                      labels = c("ERA5 area-averaged", "SRTM area-averaged", "SRTM population-weighted"))

# Compute means for each variable
means <- df %>%
  group_by(variable) %>%
  summarise(mean_val = mean(value, na.rm = TRUE))


# Plot with density and dashed mean lines
gg <- ggplot(df, aes(x = value, col = variable, fill = variable)) +
  geom_density(alpha = 0.2, linewidth = 1) +
  # Add dashed vertical lines for mean
  geom_vline(data = means, aes(xintercept = mean_val, col = variable),
             linetype = "dashed", linewidth = 1) +
  labs(
    title = country,
    x = "Health shed elevation (m)",
    y = "Density",
    fill = "Method",
    color = "Method"
  ) +
  theme_minimal()

options(scipen=999)
pdf(paste0("plots/elevation_", country, ".pdf"), width = 7, height=3)

    print(gg)
    
dev.off()
