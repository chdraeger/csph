library(arrow)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

#========================================================
country <- "Nepal"
variable <- "2m_temperature"
year <- 2024
id_name <- "fid"  # fs_uid
#========================================================

# Old: FASRC
old <- c()
for(month in 1:12){
  
  file <- paste0("~/Data/csph/ERA5/fasrc/", tolower(country), "_environmental_exposure-era5_healthshed_2m_temperature_",
                  year, "_", month, ".parquet")
  old_month <- read_parquet(file)
  old_month$geometry <- NULL
  old_month <- as.data.frame(old_month)
  
  old_month <- old_month %>%
    pivot_longer(
      cols = starts_with("day_"),      # all day_* columns
      names_to = c("day", "stat"),     # split into day number and stat type
      names_pattern = "day_(\\d{2})_daily_(.*)", 
      values_to = "value"
    ) %>% as.data.frame()
  old_month$day <- paste0(year, "-", str_pad(month, 2, pad = "0"), "-", old_month$day)
  
  old <- rbind(old, old_month)
  
}

# New: population weighted
new <- read.csv(paste0("~/Data/csph/ERA5/", country, "/era5_sl_", variable, "_", year, "_pop_weighted.csv"))
new <- reshape2::melt(new, id = c(id_name, "day"), variable.name = "stat")

#=========== 
# combine
all <- rbind(
  cbind(old, type = "old"),
  cbind(new, type = "new")
)

ggplot(all, aes(x = value, color = type, fill = type)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ stat, scales = "free") +
  labs(
    x = "Value",
    y = "Density",
    color = "Dataset",
    fill  = "Dataset"
  ) +
  theme_minimal(base_size = 14)

#######

colnames(old)[colnames(old) == "value"] <- "value_old"
colnames(new)[colnames(new) == "value"] <- "value_new"

df <- left_join(old, new, by = c("fid", "day", "stat"))
df$diff <- df$value_new - df$value_old

summary <- df %>% group_by(stat) %>%
  summarize(mean = mean(diff))




ggplot(summary, aes(x = mean)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ stat, scales = "free") +
  labs(
    x = "Value",
    y = "Density",
    color = "Dataset",
    fill  = "Dataset"
  ) +
  theme_minimal(base_size = 14)
