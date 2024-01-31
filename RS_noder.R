# noder utanför Dalarna 

#Ett skript som skapar rumslig strategi
#Allt snyggt och imponerande nedan är skapat av min bästa kompis GPT4

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
               tidyr)

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
# =========================== läs in lager ==================

offentlig_service_fil <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/Offentlig_service/hanna_praktikant/offentlig_service.csv"
# Offentlig service (sjukvård, Utbudspunkter)
sjukvard_fil <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/Offentlig_service/Utbudspunkter.csv"
# Sysselsättning
befolkning_fil <- "H:/aldhen/SuperCross/bef_100/100_meters_dag_natt_bef.gpkg"               # ersätt med 100 meterruta
# grans_dagbef <- 100 
# Komersiell service nedladdat från Pipos Serviceanalys 2023-03, 8 st CSV-filer
kom_serv_filer <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/FrånPipos/"

funktionella_orter_fil <- "funktionella_orter.gpkg"

st_layers(funktionella_orter_fil)
funktionella_orter <- st_read(funktionella_orter_fil, layer = "funktionella_orter", crs = 3006)
names(funktionella_orter)
funktionella_orter <- funktionella_orter%>% 
  select(!funk_ort.x, !funk_ort.y, priority)
mapview(funktionella_orter)
# ============= Rumsligstrategi Handel  =============================================================

# lägg till antal dagligvaror, drivmedel och post samt apotek

# En funktion som läser och konverterar CSV till sf-objekt
read_and_convert_to_sf <- function(file_path, x_col, y_col, crs, service_abbr, service_category) {
  df <- read.csv(file_path, header = TRUE, sep = ";", fileEncoding = "ISO-8859-1") %>%
    st_as_sf(coords = c(x_col, y_col), crs = crs, agr = "constant")
  return(df)
}

# Assuming the read and convert function works as intended for your files, now directly create sf objects
apotek_sf <- read_and_convert_to_sf(paste0(kom_serv_filer, "apoteksvaror.csv"), "x", "y", 3006, "APK", "basic")
dagligvaror_fullsort_sf <- read_and_convert_to_sf(paste0(kom_serv_filer, "dagligvaror_fullsortiment.csv"), "x", "y", 3006, "DVF", "commercial")
drivmedel_personbil_sf <- read_and_convert_to_sf(paste0(kom_serv_filer, "drivmedel_personbil.csv"), "x", "y", 3006, "DMP", "commercial")
posttjanster_sf <- read_and_convert_to_sf(paste0(kom_serv_filer, "posttjänster.csv"), "x", "y", 3006, "PST", "basic")

# Add service_category column to each dataframe
apotek_sf$service_category <- "apoteksvaror"
dagligvaror_fullsort_sf$service_category <- "dagligvaror_fullsortiment"
drivmedel_personbil_sf$service_category <- "drivmedel_personbil"
posttjanster_sf$service_category <- "posttjänster"

# Combine all individual sf objects into one sf object
all_services_sf <- bind_rows(
  apotek_sf,
  dagligvaror_fullsort_sf,
  drivmedel_personbil_sf,
  posttjanster_sf
)

# This step ensures that the combined object retains its spatial properties
all_services_sf <- st_as_sf(all_services_sf)

# Perform spatial join with funktionella_orter
all_services_in_orter <- st_join(all_services_sf, funktionella_orter, join = st_within)
#städa bort några kolumner
all_services_in_orter <- all_services_in_orter %>% 
  select(-c(antal.70...5.km, 
            trans.behov.70...5.km,
            kortast.rutt.70...5.km,
            antal.70...10.km,
            trans.behov.70...10.km,
            kortast.rutt.70...10.km))

# Aggregate service information
service_aggregates <- all_services_in_orter %>%
  group_by(unique_id) %>%
  summarise(
    n_handel = n(),
    sum_dagligvaror = sum(service_category == "dagligvaror_fullsortiment"),
    sum_drivmedel = sum(service_category == "drivmedel_personbil"),
    sum_post = sum(service_category == "posttjänster"),
    sum_apotek = sum(service_category == "apoteksvaror"),
    n_unik_typ = n_distinct(service_category)
  )
# Identify and filter 'funktionella_orter' with at least one of each service type

# Join the aggregates back to the funktionella_orter sf object
funktionella_orter_kommersiell_service <- st_join(funktionella_orter, service_aggregates, by = "unique_id")

# 'labelText' är en kolumn i dina sf-objekt som innehåller den text du vill visa i popups
apotek_sf$labelText <- "Apotekstjänst"
dagligvaror_fullsort_sf$labelText <- "Dagligvaror Fullsortiment"
drivmedel_personbil_sf$labelText <- "Drivmedel för Personbil"
posttjanster_sf$labelText <- "Posttjänster"
funktionella_orter_kommersiell_service$labelText <- paste("Detta är en funktionell ort. Ett experiment och ett försök att inkludera hela Dalarna, ",
                                                          "istället för att endast analysera aktivitet i tätorter. Funktionella orter består av tätorter, ",
                                                          " småorter, fritidshusområden och anläggningsområden", sep = "<br>")

mapview(funktionella_orter_kommersiell_service)+
  mapview(apotek_sf)+
  mapview(dagligvaror_fullsort_sf)+
  mapview(drivmedel_personbil_sf)+
  mapview(posttjanster_sf)

# ===================== Rumslig strategi sjukvård ==========
# Läs in data
sjukvard <- read.csv(sjukvard_fil, header = TRUE, sep = ";", fileEncoding = "ISO-8859-1")
# Ersätt kommatecken med punkter i koordinatkolumnerna
sjukvard$Sweref99X <- as.numeric(gsub(",", ".", sjukvard$Sweref99X))
sjukvard$Sweref99Y <- as.numeric(gsub(",", ".", sjukvard$Sweref99Y))

# Försök konvertera till sf-objekt igen, efter att ha filtrerat bort eventuella NA-värden
sjukvard_filtered <- sjukvard %>%
  filter(!is.na(Sweref99X) & !is.na(Sweref99Y))

sjukvard_sf <- sjukvard_filtered %>%
  st_as_sf(coords = c("Sweref99Y", "Sweref99X"), crs = 3006, agr = "constant")

sjukvard_sf <- sjukvard_sf %>% 
  filter(!is.na(FghKlass))

# Perform spatial join with funktionella_orter
sjukvard_in_orter <- st_join(sjukvard_sf, funktionella_orter, join = st_within) %>% 
  rename(service_category = FghKlass)

# Aggregate service information for each funktionell_ort
# skapa klassificering skola så att det går att köra filter, minst en från klassen utbildning, gör även klassen handel och klassen sjukvard
sjukvard_aggregates <- sjukvard_in_orter %>%
  group_by(unique_id) %>%
  summarise(
    n_sjukvard = n(),
    sum_vardcentral = sum(service_category == "Vårdcentral"),
    sum_sjukhus = sum(service_category == "Sjukhus"),
    n_unik_sjukvard = n_distinct(service_category)
  )

# Join the aggregates back to the funktionella_orter sf object
funktionella_orter_sjukvard_komserv <- st_join(funktionella_orter_kommersiell_service, sjukvard_aggregates, by = "unique_id")

mapview(sjukvard_aggregates)+
  mapview(funktionella_orter_sjukvard)+
  mapview(funktionella_orter_sjukvard_komserv)

# ===================================== hämta data från enhetsregistret ================
# regional nod skola, OBS!! data insamlat av praktikant manuellt med google och eniro

offentlig_service <- read.csv(offentlig_service_fil, header = TRUE, sep = ";")

offentlig_service <- offentlig_service %>% 
  na.omit()

offentlig_service_sf <- st_as_sf(offentlig_service, coords = c("y", "x"), crs = 3006)

# Skapa en lista för att lagra de separata sf-objekten
list_sf_objects <- list()

# Loopa över alla unika värden i 'typ_service'
for (service_typ in unique(offentlig_service_sf$typ_service)) {
  # Filtrera 'offentlig_service_sf' för varje unik 'service_typ' och spara i listan
  list_sf_objects[[service_typ]] <- offentlig_service_sf %>%
    filter(typ_service == service_typ)
}

# Skapa en variabel för varje unikt sf-objekt i listan
grundskola_sf_object <- list_sf_objects[["Grundskola"]]
gymnasieskola_sf_object <- list_sf_objects[["Gymnasieskola"]]
hogskola_sf_object <- list_sf_objects[["högskola"]]

grundskola_sf_object$service_category <- "Grundskola"
gymnasieskola_sf_object$service_category <- "Gymnasieskola"
hogskola_sf_object$service_category <- "Högskola"

# Lägg till högskola_sf_object i sammanslagningen
all_schools_sf <- bind_rows(
  grundskola_sf_object,
  gymnasieskola_sf_object,
  hogskola_sf_object  # Lägg till högskolan
)

# Konvertera tillbaka till sf-objekt om det behövs
all_schools_sf <- st_as_sf(all_schools_sf)

# Utför spatial join med funktionella_orter
all_schools_in_orter <- st_join(all_schools_sf, funktionella_orter, join = st_within)

mapview(all_schools_in_orter, zcol = "typ_service")+
  mapview(funktionella_orter)

# Aggregera skolinformation för varje funktionell ort
# skapa klassificering skola så att det går att köra filter, minst en från klassen utbildning, gör även klassen handel och klassen sjukvard
school_aggregates <- all_schools_in_orter %>%
  group_by(unique_id) %>%
  summarise(
    n_utbildning = n(),
    sum_grundskola = sum(service_category == "Grundskola"),
    sum_gymnasieskola = sum(service_category == "Gymnasieskola"),
    sum_hogskola = sum(service_category == "Högskola"),  
    n_unik_skoltyp = n_distinct(service_category)
  )
# Join the aggregates back to the funktionella_orter sf object
funktionella_orter_skola_sjukv_komserv <- st_join(funktionella_orter_sjukvard_komserv, school_aggregates, by = "unique_id")
mapview(funktionella_orter_skola_sjukv_komserv)+
  mapview(all_schools_sf, zcol = "service_category")

# ===================== sysselsättning =================
# läs in 100 metersruta istället

befolkning <- st_read(befolkning_fil) |> 
  st_transform(3006) |> 
  st_make_valid()

#make centroids
dagbef_centroids <- befolkning %>% 
  select(!natt_bef) %>% 
  st_centroid()
mapview(dagbef_centroids, cex = "dag_bef")

funktionella_orter_skola_sjukv_komserv <- funktionella_orter_skola_sjukv_komserv %>% 
  select(unique_id = unique_id.x, !unique_id.x.1, !unique_id.y, !unique_id.y.1)

sysselsattning <- st_join(dagbef_centroids, funktionella_orter_skola_sjukv_komserv, join = st_within)

# Summarize workforce data for each area
sum_sysselsattning <- sysselsattning %>%
  group_by(unique_id) %>%  # Replace 'area_id' with the identifier for your areas of interest
  summarise(sum_dagbef = sum(dag_bef, na.rm = TRUE))

# Convert to a regular dataframe if it's an sf object
sum_sysselsattning_df <- as.data.frame(sum_sysselsattning)

funktionella_orter_syss_komserv_skola_sjukvard <- funktionella_orter_skola_sjukv_komserv %>%
  left_join(sum_sysselsattning_df, by = "unique_id") %>%
  mutate(sum_dagbef = ifelse(is.na(sum_dagbef), NA, sum_dagbef))
mapview(funktionella_orter_syss_komserv_skola_sjukvard)

nattbef_centroids <- befolkning %>% 
  select(!dag_bef) %>% 
  st_centroid()

natt_befolkning <- st_join(nattbef_centroids, funktionella_orter_syss_komserv_skola_sjukvard, join = st_within)

# Summarize workforce data for each area
sum_natt_bef <- natt_befolkning %>%
  group_by(unique_id) %>%  # Replace 'area_id' with the identifier for your areas of interest
  summarise(sum_nattbef = sum(natt_bef, na.rm = TRUE))

# Convert to a regular dataframe if it's an sf object
sum_natt_befolkning_df <- as.data.frame(sum_natt_bef)

funktionella_orter_nattbef_syss_sjukv_skola_komserv <- funktionella_orter_syss_komserv_skola_sjukvard %>%
  left_join(sum_natt_befolkning_df, by = "unique_id") %>%
  mutate(sum_nattbef = ifelse(is.na(sum_nattbef), NA, sum_nattbef))
mapview(funktionella_orter_nattbef_syss_sjukv_skola_komserv)

noder_dalarna <- funktionella_orter_nattbef_syss_sjukv_skola_komserv


#skapa poängsättning
# 3 poäng för sjukvard; sjukhus, vårdcentral och apotek
# 3 poäng för skola; högskola, gymnasium och grundskola
# 3 poäng för handel; drivmedel, dagligvaror, post


# storregionalnod <- Falun och Borlänge (eller minst sjukhus)

storregionalnod <- noder_dalarna %>%
  filter(sum_drivmedel >= 1, 
         sum_post >= 1, 
         sum_dagligvaror >= 1, 
         sum_sjukhus >= 1, 
         sum_vardcentral >= 1,
         sum_apotek >= 1,
         sum_grundskola >= 1,
         sum_gymnasieskola >= 1,
         sum_hogskola >= 1) %>%
  st_centroid()

mapview(storregionalnod)

#skapa ett reverserat urval (!=) som gör att storregionalnod (raden/observationen) försvinner från funktionella_orter_utan_storregionalnod
# så att nästa ser ut som följande
# Assuming storregionalnod and noder_dalarna both have a column named unique_id

# To exclude storregionalnod from noder_dalarna
funktionella_orter_utan_storregionalnod <- anti_join(noder_dalarna, storregionalnod, by = "unique_id")

# regionalnod <- funktionella_orter_utan_storregionalnod %>% 
#   xxxx

# minst servicegrad 9
regionalnod <- noder_dalarna %>%
  filter(sum_drivmedel >= 1, 
         sum_post >= 1, 
         sum_dagligvaror >= 1, 
         sum_vardcentral >= 1,
         sum_apotek >= 1,
         sum_grundskola >= 1,
         sum_gymnasieskola >= 1,
         n_utbildning >= 3,
         n_handel >= 3,
         n_sjukvard >=3) %>% #är apoteken med här i klassen n_sjukvard?
  # servicegrad >= 9
  st_centroid()

mapview(regionalnod)+
  mapview(storregionalnod)

# delregionalnod <- servicegrad 6 minst gymnasium, minst vårdcentral, minst dagligvaror
delregionalnod <- noder_dalarna %>% 
  filter(sum_dagligvaror >= 1, 
         sum_vardcentral >= 1,
         sum_gymnasieskola >= 1,
         n_utbildning >= 2,
         n_handel >= 2,
         n_sjukvard >= 2) %>% 
  # servicegrad >= 6
  st_centroid()

mapview(delregionalnod)+
  mapview(regionalnod)+
  mapview(storregionalnod)

# närgeografisknod <- "Ort med servicegrad om minst 3 utifrån handel, vård eller utbildning ( en av varje). Har en 
# relativt hög andel arbetstillfällen (kvot över 20 procent utifrån befolkning)"
nargeografisknod <- noder_dalarna %>%
  # Lägg till en ny kolumn för kvoten mellan arbetsplatser och befolkning
  mutate(kvot_arbetsplatser_befolkning = sum_dagbef / befolkning) %>% # ändra till nattbef
  # Filtrera baserat på den nya kvoten
  filter(kvot_arbetsplatser_befolkning >= 0.20) %>%
  filter(n_utbildning >= 1,
         n_handel >= 1,
         n_sjukvard >=1)
# servicegrad >= 3

mapview(nargeografisknod)+
  mapview(delregionalnod)+
  mapview(regionalnod)+
  mapview(storregionalnod)

# boendeortmed viss service <- En boendeort med en servicegrad om minst 3( en av varje).
boendeort_service <- noder_dalarna %>%
  filter(n_utbildning >= 1,
         n_handel >= 1,
         n_sjukvard >=1)
# servicegrad >= 3

mapview(boendeort_service)+
  mapview(nargeografisknod)+
  mapview(delregionalnod)+
  mapview(regionalnod)+
  mapview(storregionalnod)

# boendeort med minst 200 personer i nattbef
boendeort <- noder_dalarna %>% 
  filter(nattbef >= 200)

mapview(boendeort, col.regions = "darkred", cex = 3)+
  mapview(boendeort_service, col.regions = "yellow", cex = 5)+
  mapview(nargeografisknod, col.regions = "darkblue", cex = 7)+
  mapview(delregionalnod, col.regions = "pink", cex = 12)+
  mapview(regionalnod, col.regions = "red", cex = 16)+
  mapview(storregionalnod, col.regions = "red", cex = 20)

library(dplyr)
library(sf)
# Assuming mapview is also used
library(mapview)

# Initial step: Identify storregionalnod based on service criteria
storregionalnod <- noder_dalarna %>%
  filter(sum_drivmedel >= 1, 
         sum_post >= 1, 
         sum_dagligvaror >= 1, 
         sum_sjukhus >= 1, 
         sum_vardcentral >= 1,
         sum_apotek >= 1,
         sum_grundskola >= 1,
         sum_gymnasieskola >= 1,
         sum_hogskola >= 1) %>%
  st_centroid()

mapview(storregionalnod)

# Exclude storregionalnod from noder_dalarna for the next step
exclude_ids <- storregionalnod$unique_id
noder_dalarna_filtered1 <- noder_dalarna %>%
  filter(!(unique_id %in% exclude_ids))

# Step 2: Identify regionalnod
regionalnod <- noder_dalarna_filtered1 %>%
  filter(sum_drivmedel >= 1, 
         sum_post >= 1, 
         sum_dagligvaror >= 1, 
         sum_vardcentral >= 1,
         sum_apotek >= 1,
         sum_grundskola >= 1,
         sum_gymnasieskola >= 1,
         n_utbildning >= 3,
         n_handel >= 3,
         n_sjukvard >=3) %>%
  st_centroid()

mapview(regionalnod) + mapview(storregionalnod)

# Update the exclusion list and filter noder_dalarna again
exclude_ids <- c(exclude_ids, regionalnod$unique_id)
noder_dalarna_filtered2 <- noder_dalarna %>%
  filter(!(unique_id %in% exclude_ids))

# Step 3: Identify delregionalnod
delregionalnod <- noder_dalarna_filtered2 %>%
  filter(sum_dagligvaror >= 1, 
         sum_vardcentral >= 1,
         sum_gymnasieskola >= 1,
         n_utbildning >= 2,
         n_handel >= 2,
         n_sjukvard >= 2) %>%
  st_centroid()

mapview(delregionalnod, col.regions = "blue") + 
  mapview(regionalnod, col.regions = "red") + 
  mapview(storregionalnod, col.regions = "green")

# Update the exclusion list and filter noder_dalarna again
exclude_ids <- c(exclude_ids, delregionalnod$unique_id)
noder_dalarna_filtered3 <- noder_dalarna %>%
  filter(!(unique_id %in% exclude_ids))

# Step 4: Identify nargeografisknod
nargeografisknod <- noder_dalarna_filtered3 %>%
  mutate(kvot_arbetsplatser_befolkning = sum_dagbef / befolkning) %>%
  filter(kvot_arbetsplatser_befolkning >= 0.20) %>%
  filter(n_utbildning >= 1,
         n_handel >= 1,
         n_sjukvard >=1) %>%
  st_centroid()

mapview(delregionalnod, col.regions = "blue") + 
  mapview(regionalnod, col.regions = "red") + 
  mapview(storregionalnod, col.regions = "green")+
  mapview(nargeografisknod)
# Update the exclusion list and filter noder_dalarna again
exclude_ids <- c(exclude_ids, nargeografisknod$unique_id)
noder_dalarna_filtered4 <- noder_dalarna %>%
  filter(!(unique_id %in% exclude_ids))

# Step 5: Identify boendeort_service and boendeort
boendeort_service <- noder_dalarna_filtered4 %>%
  filter(n_utbildning >= 1,
         n_handel >= 1) %>% 
  st_centroid()

mapview(delregionalnod, col.regions = "blue") + 
  mapview(regionalnod, col.regions = "red") + 
  mapview(storregionalnod, col.regions = "green") +
  mapview(nargeografisknod, col.regions = "orange") +
  mapview(boendeort_service, col.regions = "yellow") # Corrected parameter

exclude_ids <- c(exclude_ids, boendeort_service$unique_id)
noder_dalarna_filtered5 <- noder_dalarna %>%
  filter(!(unique_id %in% exclude_ids))
# Assuming there's a separate step or criteria for identifying boendeort
# Adjust the filtering criteria as necessary for your analysis
boendeort <- noder_dalarna_filtered5 %>%
  filter(sum_nattbef >= 200) %>% 
  st_centroid()

mapview(delregionalnod, col.regions = "blue") + 
  mapview(regionalnod, col.regions = "red") + 
  mapview(storregionalnod, col.regions = "green") +
  mapview(nargeografisknod, col.regions = "orange") +
  mapview(boendeort_service, col.regions = "yellow") +
  mapview(boendeort, col.regions = "black")# Corrected parameter


