
gcm <- "MPI-ESM1-2-HR"
variable <- "hurs"
country <- "Madagascar"
cmip_cell_id <- "cmip_cell_1"
ssp <- "ssp126"
variant <- "r1i1p1f1"

df <- read.csv(paste0("~/Data/csph/CMIP6/", country, "/", gcm, "/", ssp, "_", gcm, "_", variant, "_", variable, ".csv"))


plot(df$cmip_cell1)

# # Convert precipitation [kg m-2 s-1] to [mm / day]
# rho_water <- 997
# cmip$precip <- cmip$precip * 60*60*24 * 1000/rho_water