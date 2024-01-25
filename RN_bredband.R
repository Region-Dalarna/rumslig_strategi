
# =========================Bredband===================

library(keyring)
library(sf)          # classes and functions for vector data
library(httr)
library(tidyverse)
library(readxl)
library(sp)
library(mapview)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_postgis.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)

# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, 
                     username = key_list(service = "auth")$username, password = key_get("auth", key_list(service = "auth")$username)))
set_config(config(ssl_verifypeer = 0L))

# avoid scientific notation
options(scipen=999)

# G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/Bredband
#PTS_brbkart2021_byggn_korr2.gpkg

bredband_fil <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/Bredband/PTS_brbkart2021_byggn_korr2.gpkg"

st_layers(bredband_fil)

bredband <- st_read(bredband_fil, layer = "PTS_brbkart2021_byggn_korr2", crs = 3006)
# mapview(bredband)

names(bredband)
bredband <- bredband %>% 
  select(ANDAMAL_1T, uuid, kommunkod, tatortkod_21, smaortkod_15, hus_antal, arb_antal, fritidshus, fiber_absnarhet, fiber)

unique(bredband$kommunkod)
bredband_2026 <- bredband %>% 
  filter(kommunkod == "2026")|>
  st_make_valid()
# mapview(bredband_2026, zcol = "fiber_absnarhet")
# 
# To determine the percentage of buildings connected to the internet based on a binary variable fiber_tillganglighet 
# (where 1 indicates internet connection and 0 indicates no internet connection), you can follow a similar approach 
# to the script you provided, with adjustments for the specific variable and calculation you're interested in. 
# Here's how you might structure your script:

# Read your spatial data file (assuming it contains a 'fiber_tillganglighet' column)
# buildings <- bredband_2026 # endast ett urval
buildings <- bredband # endast ett urval

# Read the "funktionella_orter" spatial data
funktionella_orter <- st_read("funktionella_orter.gpkg", crs = 3006)

# If necessary, create centroids for buildings data (if they're not already centroids)
buildings_centroids <- st_centroid(buildings)

# Spatial join: match buildings to their respective "funktionella_orter"
buildings_in_orter <- st_join(buildings_centroids, funktionella_orter, join = st_within)

# Calculate the share of buildings with internet connection for each "funktionella_ort"
internet_connection_summary <- buildings_in_orter %>%
  group_by(unique_id) %>%  # Assume 'unique_id' uniquely identifies "funktionella_orter"
  summarise(
    total_buildings = n(),
    connected_buildings = sum(fiber_absnarhet, na.rm = TRUE),
    share_connected = (connected_buildings / total_buildings) * 100
  )

# Convert to a regular dataframe if it's an sf object (optional)
internet_connection_summary_df <- as.data.frame(internet_connection_summary)

# Join this summary back to the "funktionella_orter" spatial data for mapping
funktionella_orter_with_internet_share <- funktionella_orter %>%
  left_join(internet_connection_summary_df, by = "unique_id")

# Visualize on a map
# mapview(funktionella_orter_with_internet_share, zcol = "share_connected", legend = TRUE)+
#   mapview(bredband_2026, zcol = "fiber_absnarhet")+
#   mapview(buildings_centroids, zcol = "fiber_absnarhet")

regionalnod_bredband <- funktionella_orter_with_internet_share %>% 
  st_centroid() %>% 
  filter(share_connected >=60)

# %>% 
#   filter(share_connected >= 60)
# mapview(regionalnod_bredband, zcol = "share_connected", cex = "share_connected")+
#   mapview(regionalnod_sysselsattning, zcol = "sum_dagbef", cex = "sum_dagbef")+
#   mapview(tatort_layer, col.regions = "blue", alpha.regions = 0.3)
# 
# mapview(regionalnod_bredband, col.regions = "green", cex = 5)+
#   mapview(regionalnod_sysselsattning, col.regions = "orange", cex = 3)+
#   mapview(tatort_layer, col.regions = "blue", alpha.regions = 0.2)+
#   mapview(funktionella_orter, col.regions = "lightblue", alpha.regions = 0.2)
# 
# st_write(funktionella_orter_with_internet_share, "funktionella_orter.gpkg", layer = "funktionella_orter_bredband", driver = "GPKG")
# 
