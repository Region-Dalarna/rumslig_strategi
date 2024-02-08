# libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               sf, 
               sp, 
               httr, 
               mapview, 
               leaflet, 
               readxl, 
               keyring,
               DBI,
               units,
               dplyr,
               tidyr,
               readr,
               stringr)

options(dplyr.summarise.inform = FALSE)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_postgis.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, 
                     username = key_list(service = "auth")$username, password = key_get("auth", key_list(service = "auth")$username)))
set_config(config(ssl_verifypeer = 0L))

# avoid scientific notation
options(scipen=999)
# ===================================================================================
# fil <- "C:/Users/henri/data_fran_G/"
#G:\Samhällsanalys\GIS\grundkartor\smaorter
fil <- "G:/Samhällsanalys/GIS/grundkartor/"

smaort <- "smaorter/Smaorter_1990_2020.gpkg"

# st_layers(paste0(fil, smaort))

smaort_layer <- st_read(paste0(fil, smaort), layer = "So2020_SR99TM", crs = 3006)

smaort_layer <- smaort_layer %>%  
  filter(LAN == "20")

# hämta dag_natt_smaort.csv från G:\Samhällsanalys\GIS\projekt\rumslig_strategi\data

smaort_dagbef_fil <- "G:/Samhällsanalys/GIS/projekt/rumslig_strategi/data/dag_natt_bef_smaort.csv"

smaort_dagbef <- read_delim(smaort_dagbef_fil, delim = ";", skip = 5, locale = locale(encoding = "Latin1"))

smaort_dagbef <- smaort_dagbef %>%
  mutate(SMAORT = substring(...1, 3)) %>%
  select(-...1) %>%
  filter(str_starts(SMAORT, "20"))

# joinar geopackage med CSV

smaorter <- left_join(smaort_layer, smaort_dagbef, by = "SMAORT") %>%
  select(-UUID, -LAN, -LANNAMN, -ValidFrom, -ValidTo, -AR)

smaorter <- smaorter %>% 
  mutate( dag_bef = as.numeric(Dagbefolkning),
          natt_bef = as.numeric(Nattbefolkning))

smaorter <- smaorter %>%
  select(-Dagbefolkning, -Nattbefolkning)

# hämta dag_natt_tatort.csv från G:\Samhällsanalys\GIS\projekt\rumslig_strategi\data

tatort_dagbef_fil <- "G:/Samhällsanalys/GIS/projekt/rumslig_strategi/data/dag_natt_bef_tatort.csv"

tatort_dagbef <- read_delim(tatort_dagbef_fil, delim = ";", skip = 5, locale = locale(encoding = "Latin1"))

tatort_dagbef <- tatort_dagbef %>% 
  rename(tatort_kod = "...1")

tatort_dagbef <- tatort_dagbef %>% 
  mutate( dag_bef = as.numeric(Dagbefolkning),
          natt_bef = as.numeric(Nattbefolkning))%>%
  select(-Dagbefolkning, -Nattbefolkning)

tatort <- "tatorter/Tatorter_1980_2020.gpkg"
# st_layers(paste0(fil, tatort))
tatort_path <- paste0(fil, tatort) # Detta bör vara den fullständiga sökvägen till din .gpkg-fil

tatort_layer <- st_read(tatort_path, layer = "To2020_SR99TM", crs = 3006)

tatort_layer <- tatort_layer %>%  
  dplyr::filter(LAN == "20")

# joina tätorter gpkg med tätorter dagbefolkning från MONA

tatorter <- left_join(tatort_layer, tatort_dagbef, by = "tatort_kod") %>%
  select(-UUID, -LAN, -LANNAMN, -ValidFrom, -ValidTo, -AR)

# =========================================0

fritid_omr <- "fritidshusomraden/Fo2015_Swe99TM.shp"
fritid_layer <- st_read(paste0(fil, fritid_omr), crs = 3006)
fritid_layer <- fritid_layer |> 
  dplyr::filter(LANSNAMN == "Dalarna")
mapview(fritid_layer)

fil_anl <- "G:/Samhällsanalys/GIS/Geotorget/anlaggningsomrade_ln20/"

anlag_omr <- "anlaggningsomrade_ln20.gpkg"
anlag_omr_path <- paste0(fil_anl, anlag_omr)
# st_layers(anlag_omr_path)
anlag_layer <- st_read(anlag_omr_path, layer = "anlaggningsomrade", crs = 3006)
mapview(anlag_layer, zcol = "andamal")
anlag_layer <- anlag_layer %>% 
  filter(objekttyp != "Idrottsplan",
         andamal != "Idrottsanläggning",
         andamal != "Begravningsplats",
         andamal != "Koloniområde",
         andamal != "Skjutbaneområde")

buffer = 300

# Buffra varje lager
tatort_buffer <- st_buffer(tatorter, dist = buffer)
smaort_buffer <- st_buffer(smaorter, dist = buffer)
fritid_buffer <- st_buffer(fritid_layer, dist = buffer)
anlag_buffer <- st_buffer(anlag_layer, dist = buffer)

# Lägg till prioritet
tatort_buffer$priority <- 1
smaort_buffer$priority <- 2
fritid_buffer$priority <- 3
anlag_buffer$priority <- 4

# fixar kolumner så att alla lager ser lika ut i uppsättningen  

funk_tatort <- tatort_buffer |> 
  dplyr::select(TATORT, KOMMUNNAMN, KOMMUN, BEF, dag_bef, natt_bef, tatort_kod, geom, priority) |> 
  rename(namn = TATORT, 
         kommun = KOMMUN, 
         kommunnamn = KOMMUNNAMN, 
         befolkning = BEF,
         kod = tatort_kod) |> 
  mutate(omr = "Tätort")

funk_smaort <- smaort_buffer |> 
  dplyr::select(SMAORT, KOMMUNNAMN, KOMMUN, BEF, dag_bef, natt_bef, geom, priority) |> 
  rename(kod = SMAORT, 
         kommun = KOMMUN, 
         kommunnamn = KOMMUNNAMN, 
         befolkning = BEF) |> 
  mutate(namn = NA, omr = "Småort")

funk_fritid <- fritid_buffer |> 
  dplyr::select(FRITIDSHUS, KOMMUNNAMN, KOMMUNKOD, geometry, priority) |> 
  rename(kod = FRITIDSHUS, 
         kommun = KOMMUNKOD, 
         kommunnamn = KOMMUNNAMN,
         geom = geometry) |> 
  mutate(befolkning = NA, namn = NA, omr = "Fritidshusområde", dag_bef = NA, natt_bef = NA)

funk_anlag <- anlag_buffer |> 
  dplyr::select(objekttyp, andamal, geom, priority) |> 
  mutate(namn = paste(objekttyp, andamal, sep = ", ")) |>
  select(namn, geom, priority) |>
  mutate(befolkning = NA, kommun = NA, kommunnamn = NA, kod = NA, omr = "Anläggningsområde", dag_bef = NA, natt_bef = NA) 

# standard_order <- c("omr", "kod", "namn", "kommunnamn", "kommun", "befolkning", "priority", "geom")
# 
# funk_anlag <- funk_anlag[standard_order]
# funk_smaort <- funk_smaort[standard_order]
# funk_tatort <- funk_tatort[standard_order]
# funk_fritid <- funk_fritid[standard_order]

funk_ort <- rbind(funk_tatort, funk_smaort, funk_fritid, funk_anlag) |>
  mutate(funk_ort = "funk_ort")

funk_ort_diss <- funk_ort %>%
  st_make_valid() %>%
  group_by(funk_ort) %>%
  summarize(geom = st_union(geom))

# Step 3: Generate single polygons from multipolygons if needed
single_polygons <- st_cast(funk_ort_diss, "POLYGON")

# Skapa centroider för funk_ort
funk_ort_centroids <- st_centroid(funk_ort)

# Step 1: Spatial Join
joined_data <- st_join(single_polygons, funk_ort_centroids)

# Step 2 & 3: Sorting and Filtering
# Assuming 'priority' and 'population' are the column names in 'centroids'
processed_data <- joined_data %>%
  arrange(priority, desc(befolkning)) %>%
  group_by(geom) %>%
  slice(1) %>%
  ungroup()

# Step 4: Cleanup
# Removing duplicates and keeping necessary columns
final_data <- processed_data %>%
  distinct(geom, .keep_all = TRUE)

# Adding a unique ID column to the final data
final_data_with_id <- final_data %>%
  mutate(unique_id = row_number())

# View the updated dataframe
glimpse(final_data_with_id)


mapview(final_data_with_id)+
  mapview(smaorter, col.regions = "red") + 
  mapview(tatorter, col.regions = "blue") + 
  mapview(fritid_layer, col.regions = "green") + 
  mapview(anlag_layer, col.regions = "yellow") + 
  mapview(funk_ort_centroids)

funktionella_orter <- final_data_with_id

# st_write(funktionella_orter, "funktionella_orter.gpkg", layer = "funktionella_orter", driver = "GPKG")
# G:\Samhällsanalys\GIS\projekt\rumslig_strategi\data
st_write(funktionella_orter, "G:/Samhällsanalys/GIS/projekt/rumslig_strategi/data/funktionella_orter.gpkg", layer = "funktionella_orter", driver = "GPKG", append=FALSE)














