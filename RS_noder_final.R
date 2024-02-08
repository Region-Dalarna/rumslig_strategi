# Testar att köra med funktionella_orter med alla fyra 
# först sen joinar dessa istället för att bygga på efterhand som i skriptet RS_noder


# noder utanför Dalarna, vilken data behövs?
# kör med tätort och småort koppla dag och natt bef till funktionella tät och små ort först
# ersätt skola med data från skolverkets enhetsregister


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
# befolkning_fil <- "H:/aldhen/SuperCross/bef_100/100_meters_dag_natt_bef.gpkg"               # ersätt med 100 meterruta

# sökvägen är G:\Samhällsanalys\GIS\projekt\rumslig_strategi\data
befolkning_fil <- "G:/Samhällsanalys/GIS/projekt/rumslig_strategi/data/dag_natt_bef_500.gpkg"

# grans_dagbef <- 100 
# Komersiell service nedladdat från Pipos Serviceanalys 2023-03, 8 st CSV-filer
kom_serv_filer <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/FrånPipos/"

funktionella_orter_fil <- "funktionella_orter.gpkg"

st_layers(funktionella_orter_fil)
funktionella_orter <- st_read(funktionella_orter_fil, layer = "funktionella_orter", crs = 3006)
names(funktionella_orter)
funktionella_orter <- funktionella_orter%>% 
  select(-funk_ort.x, -funk_ort.y, -priority)

print(length(unique(funktionella_orter$unique_id)))
# mapview(funktionella_orter)


# ============= Rumsligstrategi Handel  (+ apotek, som hör till=============================================================
# ========== data från Pipos Service analys ===================

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

dagligvaror_fullsort_sf$labelText <- "Dagligvaror Fullsortiment"
drivmedel_personbil_sf$labelText <- "Drivmedel för Personbil"
posttjanster_sf$labelText <- "Posttjänster"
apotek_sf$labelText <- "Apotekstjänst"


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
# Assuming all_services_in_orter is your combined sf data frame that includes service_category

all_services_in_orter <- all_services_in_orter %>%
  mutate(klass = case_when(
    service_category %in% c("dagligvaror_fullsortiment", "drivmedel_personbil", "posttjänster") ~ "handel",
    service_category == "apoteksvaror" ~ "sjukvard",
    TRUE ~ service_category # This line might need adjustment
  ))



# Aggregate service information
service_aggregates <- all_services_in_orter %>%
  group_by(unique_id) %>%
  summarise(
    sum_dagligvaror = sum(service_category == "dagligvaror_fullsortiment"),
    sum_drivmedel = sum(service_category == "drivmedel_personbil"),
    sum_post = sum(service_category == "posttjänster"),
    n_handel = sum(klass == "handel"),
    sum_apotek = sum(service_category == "apoteksvaror")
  )
# Identify and filter 'funktionella_orter' with at least one of each service type

# Join the aggregates back to the funktionella_orter sf object
funktionella_orter_kom_service <- st_join(funktionella_orter, service_aggregates, by = "unique_id")

funktionella_orter_kom_service <- funktionella_orter_kom_service %>% 
  rename(unique_id = unique_id.x) %>% 
  select(-unique_id.y)
# # Check if there are any NA values in the unique_id column

any(is.na(funktionella_orter_kom_service$unique_id))
sum(is.na(funktionella_orter_kom_service$unique_id))
print(length(unique(funktionella_orter_kom_service$unique_id)))
mapview(funktionella_orter_kom_service)


#      
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

sjukvard_in_orter <- sjukvard_in_orter %>% 
  mutate(klass = case_when(
    service_category %in% c("Vårdcentral", "Sjukhus") ~ "sjukvard",
    TRUE ~ service_category
  ))

# Aggregate service information for each funktionell_ort
# skapa klassificering skola så att det går att köra filter, minst en från klassen utbildning, gör även klassen handel och klassen sjukvard
sjukvard_aggregates <- sjukvard_in_orter %>%
  group_by(unique_id) %>%
  summarise(
    sum_vardcentral = sum(service_category == "Vårdcentral"),
    sum_sjukhus = sum(service_category == "Sjukhus")
  )

# Join the aggregates back to the funktionella_orter sf object
funktionella_orter_sjukvard <- st_join(funktionella_orter, sjukvard_aggregates, by = "unique_id")

funktionella_orter_sjukvard <- funktionella_orter_sjukvard %>%
  rowwise() %>%
  ungroup() %>% 
  rename(unique_id = unique_id.y) %>% 
  select(-unique_id.x)

names(funktionella_orter_sjukvard)# 
any(is.na(funktionella_orter_sjukvard$unique_id))
sum(is.na(funktionella_orter_sjukvard$unique_id))
print(length(unique(funktionella_orter_sjukvard$unique_id)))

mapview(sjukvard_sf, zcol = "FghKlass", label = "Populärnamn")+ 
  mapview(funktionella_orter_sjukvard)


# ===================================== hämta data från enhetsregistret ================
# regional nod skola, OBS!! data insamlat av praktikant manuellt med google och eniro
# encoding 
offentlig_service <- read_delim(offentlig_service_fil, delim = ";", locale = locale(encoding = "ISO-8859-1"))

# Check if the special characters are displayed correctly
head(offentlig_service)

library(dplyr)

offentlig_service <- offentlig_service %>%
  filter(!is.na(x) & !is.na(y))


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
all_schools_in_orter <- st_join(all_schools_sf, funktionella_orter, join = st_within)%>% 
  mutate(klass = case_when(
    service_category %in% c("Grundskola", "Gymnasieskola", "Högskola") ~ "utbildning",
    TRUE ~ service_category
  ))

# mapview(all_schools_in_orter, zcol = "typ_service")+
#   mapview(funktionella_orter)

# Aggregera skolinformation för varje funktionell ort
# skapa klassificering skola så att det går att köra filter, minst en från klassen utbildning, gör även klassen handel och klassen sjukvard
school_aggregates <- all_schools_in_orter %>%
  group_by(unique_id) %>%
  summarise(
    sum_grundskola = sum(service_category == "Grundskola"),
    sum_gymnasieskola = sum(service_category == "Gymnasieskola"),
    sum_hogskola = sum(service_category == "Högskola"),  
    n_skola = sum(klass == "utbildning")
  )
# Join the aggregates back to the funktionella_orter sf object
funktionella_orter_skola <- st_join(funktionella_orter, school_aggregates, by = "unique_id")


funktionella_orter_skola <- funktionella_orter_skola %>%
  select(-unique_id.x) %>% 
  rename(unique_id = unique_id.y)

mapview(funktionella_orter_skola)+
  mapview(all_schools_sf, zcol = "service_category")

names(funktionella_orter_skola)# 
any(is.na(funktionella_orter_skola$unique_id))
sum(is.na(funktionella_orter_skola$unique_id))
print(length(unique(funktionella_orter_skola$unique_id)))


# ===================== sysselsättning =================
# läs in 100 metersruta istället

befolkning <- st_read(befolkning_fil) |>
  st_transform(3006) |>
  st_make_valid()
mapview(befolkning)

#make centroids
dagbef_centroids <- befolkning %>%
  select(!natt_bef) %>%
  st_centroid() %>% 
  filter(!is.na(dag_bef))

sysselsattning <- st_join(dagbef_centroids, funktionella_orter, join = st_within)

# Summarize workforce data for each area
sum_sysselsattning <- sysselsattning %>%
  group_by(unique_id) %>%  # Replace 'area_id' with the identifier for your areas of interest
  summarise(sum_dagbef = sum(dag_bef.x, na.rm = TRUE))

# Convert to a regular dataframe if it's an sf object
sum_sysselsattning_df <- as.data.frame(sum_sysselsattning)

funktionella_orter_syss <- funktionella_orter %>%
  left_join(sum_sysselsattning_df, by = "unique_id") %>%
  mutate(sum_dagbef = ifelse(is.na(sum_dagbef), NA, sum_dagbef))

mapview(funktionella_orter_syss)

names(funktionella_orter_syss)# 
any(is.na(funktionella_orter_syss$unique_id))
sum(is.na(funktionella_orter_syss$unique_id))
print(length(unique(funktionella_orter_syss$unique_id)))

nattbef_centroids <- befolkning %>%
  select(!dag_bef) %>%
  st_centroid()

natt_befolkning <- st_join(nattbef_centroids, funktionella_orter, join = st_within)

# Summarize workforce data for each area
sum_natt_bef <- natt_befolkning %>%
  group_by(unique_id) %>%  # Replace 'area_id' with the identifier for your areas of interest
  summarise(sum_nattbef = sum(natt_bef.x, na.rm = TRUE))

# Convert to a regular dataframe if it's an sf object
sum_natt_befolkning_df <- as.data.frame(sum_natt_bef)

funktionella_orter_nattbef <- funktionella_orter %>%
  left_join(sum_natt_befolkning_df, by = "unique_id") %>%
  mutate(sum_nattbef = ifelse(is.na(sum_nattbef), NA, sum_nattbef))

mapview(funktionella_orter_nattbef)

names(funktionella_orter_nattbef)# 
any(is.na(funktionella_orter_nattbef$unique_id))
sum(is.na(funktionella_orter_nattbef$unique_id))
print(length(unique(funktionella_orter_nattbef$unique_id)))


# joina alla funktionella orter
# glöm inte denna!
# mutate(n_sjukvard = sum(c(sum_apotek, sum_vardcentral, sum_sjukhus), na.rm = TRUE)) 
#
#n_sjukvard = sum(klass == "sjukvard")
# =================  join av funktionella orter ========================

# Starting with the original dataset
result <- funktionella_orter

# Join with sjukvard and select desired columns
result <- result %>%
  st_join(funktionella_orter_sjukvard %>% select(unique_id, sum_vardcentral, sum_sjukhus), by = "unique_id")

# Join with syss and select desired columns
result <- result %>%
  st_join(funktionella_orter_syss %>% select(unique_id, sum_dagbef), by = "unique_id")

# Join with nattbef and select desired columns
result <- result %>%
  st_join(funktionella_orter_nattbef %>% select(unique_id, sum_nattbef), by = "unique_id")

# Join with skola and select desired columns
result <- result %>%
  st_join(funktionella_orter_skola %>% select(unique_id, sum_grundskola, sum_gymnasieskola, sum_hogskola), by = "unique_id")

# Join with kom_service and select desired columns
result <- result %>%
  st_join(funktionella_orter_kom_service %>% select(unique_id, sum_dagligvaror, sum_drivmedel, sum_post, sum_apotek), by = "unique_id")

strategiska_noder <- result# Now 'result' contains all the joined data

strategiska_noder <- strategiska_noder %>%
  # Assuming unique_id.x does not contain NA values and is the correct unique identifier
  select(-unique_id.y, -unique_id.x.1, -unique_id.y.1, -unique_id.x.2, -unique_id.y.2) %>%
  rename(unique_id = unique_id.x,
         befolkning_ort = befolkning,
         dag_bef_MONA = dag_bef,
         natt_bef_MONA = natt_bef,
         sum_dagbef_ruta = sum_dagbef,
         sum_nattbef_ruta = sum_nattbef) %>% 
  mutate(
    n_sjukvard = sum_vardcentral + sum_sjukhus + sum_apotek,
    n_skola = sum_grundskola + sum_gymnasieskola + sum_hogskola,
    n_handel = sum_dagligvaror + sum_drivmedel + sum_post
  )

mapview(strategiska_noder)
st_write(strategiska_noder, "G:/Samhällsanalys/GIS/projekt/rumslig_strategi/data/funktionella_orter.gpkg", 
         layer = "funktionella_orter_egenskaper", driver = "GPKG", append=TRUE)

# ===================================================================================

# Initial step: Identify storregionalnod based on service criteria
storregionalnod <- strategiska_noder %>%
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
 
# mapview(storregionalnod)

# Step 2: Identify regionalnod
regionalnod <- strategiska_noder %>%
  filter(sum_drivmedel >= 1, 
         sum_post >= 1, 
         sum_dagligvaror >= 1, 
         sum_vardcentral >= 1,
         sum_apotek >= 1,
         sum_grundskola >= 1,
         sum_gymnasieskola >= 1,
         n_skola >= 3,
         n_handel >= 3,
         n_sjukvard >=3) %>%
  st_centroid()

# mapview(regionalnod) + mapview(storregionalnod)

# Step 3: Identify delregionalnod
delregionalnod <- strategiska_noder %>%
  filter(
    # sum_dagligvaror >= 1,
    #      sum_vardcentral >= 1,
    #      sum_gymnasieskola >= 1,
    n_skola >= 2,
    n_handel >= 2,
    n_sjukvard >= 2) %>%
  st_centroid()

mapview(delregionalnod, col.regions = "blue") +
  mapview(regionalnod, col.regions = "red") +
  mapview(storregionalnod, col.regions = "green")

# Step 4: Identify nargeografisknod
nargeografisknod <- strategiska_noder %>%
  mutate(kvot_arbetsplatser_befolkning = sum_dagbef_ruta / sum_nattbef_ruta) %>%
  filter(kvot_arbetsplatser_befolkning >= 0.20) %>%
  filter(n_skola >= 1,
         n_handel >= 1,
         n_sjukvard >=1) %>%
  st_centroid()

mapview(delregionalnod, col.regions = "blue") +
  mapview(regionalnod, col.regions = "red") +
  mapview(storregionalnod, col.regions = "green")+
  mapview(nargeografisknod)
# # Update the exclusion list and filter noder_dalarna again
# exclude_ids <- c(exclude_ids, nargeografisknod$unique_id)
# noder_dalarna_filtered4 <- noder_dalarna %>%
#   filter(!(unique_id %in% exclude_ids))

# Step 5: Identify boendeort_service and boendeort
boendeort_service <- strategiska_noder %>%
  filter(
    # n_skola >= 1,
    n_handel >= 1,
    sum_dagbef_ruta >= 200) %>% 
  st_centroid()

boendeort <- strategiska_noder %>%
  filter(sum_nattbef_ruta >= 100) %>% 
  st_centroid()

storregionalnod$filter <- paste("filter(sum_drivmedel >= 1, 
         sum_post >= 1, 
         sum_dagligvaror >= 1, 
         sum_sjukhus >= 1, 
         sum_vardcentral >= 1,
         sum_apotek >= 1,
         sum_grundskola >= 1,
         sum_gymnasieskola >= 1,
         sum_hogskola >= 1)", sep = "<br>")
regionalnod$filter <- paste("filter(sum_drivmedel >= 1, 
                                          sum_post >= 1, 
                                          sum_dagligvaror >= 1, 
                                          sum_vardcentral >= 1,
                                          sum_apotek >= 1,
                                          sum_grundskola >= 1,
                                          sum_gymnasieskola >= 1,
                                          n_skola >= 3,
                                          n_handel >= 3,
                                          n_sjukvard >=3)")
delregionalnod$filter <- paste("filter(
    # sum_dagligvaror >= 1,
    #      sum_vardcentral >= 1,
    #      sum_gymnasieskola >= 1,
         n_skola >= 2,
         n_handel >= 2,
         n_sjukvard >= 2")
nargeografisknod$filter <- paste("mutate(kvot_arbetsplatser_befolkning = sum_dagbef_ruta / sum_nattbef_ruta) %>%
  filter(kvot_arbetsplatser_befolkning >= 0.20) %>%
  filter(n_skola >= 1,
         n_handel >= 1,
         n_sjukvard >=1)")
boendeort_service$filter <- paste("filter(
    # n_skola >= 1,
    n_handel >= 1,
    sum_dagbef_ruta >= 200)")
boendeort$filter <- paste("filter(sum_nattbef_ruta >= 100)")

# lägg till typ av nod
storregionalnod$typ_av_nod <- paste("storregionalnod")
regionalnod$typ_av_nod <- paste("regionalnod")
delregionalnod$typ_av_nod <- paste("delregionalnod")
nargeografisknod$typ_av_nod <- paste("nargeografisknod")
boendeort_service$typ_av_nod <- paste("boendeort_service")
boendeort$typ_av_nod <- paste("boendeort")


mapview(strategiska_noder, alpha.regions = 0.3, lwd = 0.2, layer.name = "Experimentell platsdefinition", col.regions = "purple", label = "namn")+
  mapview(storregionalnod, col.regions = "blue", cex = 25, lwd = 0.2, popup = paste("Detta är en ", storregionalnod$typ_av_nod, "enligt definition/", storregionalnod$filter), label = "typ_av_nod") + 
  mapview(regionalnod, col.regions = "green", cex = 20, lwd = 0.2, popup = paste("Detta är en ", regionalnod$typ_av_nod, "enligt definition/", regionalnod$filter), label = "typ_av_nod") + 
  mapview(delregionalnod, col.regions = "yellow", cex = 15, lwd = 0.2, popup = paste("Detta är en ", delregionalnod$typ_av_nod, "enligt definition/", delregionalnod$filter), label = "typ_av_nod")+
  mapview(nargeografisknod, col.regions = "darkorange", cex = 10, lwd = 0.2, popup = paste("Detta är en ", nargeografisknod$typ_av_nod, "enligt definition/", nargeografisknod$filter), label = "typ_av_nod") +
  mapview(boendeort_service, col.regions = "red", cex = 6, lwd = 0.2, popup = paste("Detta är en ", boendeort_service$typ_av_nod, "enligt definition/", boendeort_service$filter), label = "typ_av_nod") +
  mapview(boendeort, col.regions = "darkred", cex = 3, lwd = 0.2, popup = paste("Detta är en ", boendeort$typ_av_nod, "enligt definition/", boendeort$filter), label = "typ_av_nod")


