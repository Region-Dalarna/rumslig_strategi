#Ett skript som skapar regional noder
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
# Funktionella orter är ett försök att inkludera hela Dalarna i analysen, inte bara aktivitet i tätorter. 
# Funktionella orter består av tätorter, småorter, fritidshusområden och anläggningsområden samt en buffer på 200 meter.
# funktionella orter är skapade i ett annat skript
# Om vi vill använda andra orter som t.ex. Tätorter ändra till tatortskod <- unique_id

funktionella_orter_fil <- "funktionella_orter.gpkg"                  # gpkg sparat i projektet finns att ladda ner från repository

# Komersiell service nedladdat från Pipos Serviceanalys 2023-03, 8 st CSV-filer
kom_serv_filer <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/FrånPipos/"

# Bredband, data från Bredbandskoordinator 2023
bredband_fil <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/Bredband/PTS_brbkart2021_byggn_korr2.gpkg"

# Offentlig service (sjukvård, Utbudspunkter)
sjukvard_fil <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/Offentlig_service/Utbudspunkter.csv"

# Sysselsättning
dagbef_fil <- "dag_natt_bef_ruta1km.gpkg"


# =============== Analysen =========================================================
# =============== Funktionella orter
funktionella_orter <- st_read("funktionella_orter_fil", crs = 3006) 

funktionella_orter <- funktionella_orter%>% 
  select(!funk_ort.y) %>% 
  rename(funk_ort = funk_ort.x, namn_ort = namn)

# ============= Analys Komersiell service =============================================================
# En funktion som läser och konverterar CSV till sf-objekt
read_and_convert_to_sf <- function(file_path, x_col, y_col, crs, service_abbr, service_category) {
  df <- read.csv(file_path, header = TRUE, sep = ";", fileEncoding = "ISO-8859-1") %>%
    mutate(service_type_abbr = service_abbr, service_category = service_category) %>%
    st_as_sf(coords = c(x_col, y_col), crs = crs, agr = "constant")
  return(df)
}

# Assuming the read and convert function works as intended for your files, now directly create sf objects
apotek_sf <- read_and_convert_to_sf(paste0(kom_serv_filer, "apoteksvaror.csv"), "x", "y", 3006, "APK", "basic")
betalnings_formedling_sf <- read_and_convert_to_sf(paste0(kom_serv_filer, "betalnings_förmedling.csv"), "x", "y", 3006, "BTF", "basic")
dagkasse_insattning_sf <- read_and_convert_to_sf(paste0(kom_serv_filer, "dagkasse_insättning.csv"), "x", "y", 3006, "DKI", "basic")
dagligvaror_EJ_fullsort_sf <- read_and_convert_to_sf(paste0(kom_serv_filer, "dagligvaror_EJ!_fullsortiment.csv"), "x", "y", 3006, "DVEJ", "basic")
dagligvaror_fullsort_sf <- read_and_convert_to_sf(paste0(kom_serv_filer, "dagligvaror_fullsortiment.csv"), "x", "y", 3006, "DVF", "commercial")
drivmedel_personbil_sf <- read_and_convert_to_sf(paste0(kom_serv_filer, "drivmedel_personbil.csv"), "x", "y", 3006, "DMP", "commercial")
posttjanster_sf <- read_and_convert_to_sf(paste0(kom_serv_filer, "posttjänster.csv"), "x", "y", 3006, "PST", "basic")
uttagsautomat_sf <- read_and_convert_to_sf(paste0(kom_serv_filer, "uttagsautomat.csv"), "x", "y", 3006, "UTA", "basic")

# Combine all individual sf objects into one sf object
all_services_sf <- bind_rows(
  apotek_sf,
  betalnings_formedling_sf,
  dagkasse_insattning_sf,
  dagligvaror_EJ_fullsort_sf,
  dagligvaror_fullsort_sf,
  drivmedel_personbil_sf,
  posttjanster_sf,
  uttagsautomat_sf
)

# Since bind_rows() might strip the sf class, re-convert to an sf object if necessary
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

# Aggregate service information for each funktionell_ort
service_aggregates <- all_services_in_orter %>%
  group_by(unique_id) %>%
  summarise(
    n_service = n(),
    sum_grund_serv = sum(service_category == "basic"),
    sum_kom_serv = sum(service_category == "commercial"),
    n_unik_grund_kom_serv = n_distinct(service_category),
    n_unik_alla_typer = n_distinct(service_type_abbr)
  )
# Identify and filter 'funktionella_orter' with at least one of each service type

# Definition: Kommersiell service samt en annan grundläggande service på platsen.   
# Med kommersiell service menas dagligvaruhandel och/eller drivmedel.  
# Med annan grundläggande service menas post-och pakettjänster, kontantuttag, betaltjänster eller  
# dagskassehantering, samt ombudstjänster för apoteksvaror.
# Join the aggregates back to the funktionella_orter sf object
funktionella_orter_kommersiell_service <- st_join(funktionella_orter, service_aggregates, by = "unique_id")

# Filtrera för att hitta orter med minst en grundläggande och en kommersiell service
regionalnod_kommersiell_service <- funktionella_orter_kommersiell_service %>%
  filter(sum_grund_serv >= 1 & sum_kom_serv >= 1)

# Beräkna centroider för dessa filtrerade orter
regionalnod_kommersiell_service <- st_centroid(regionalnod_kommersiell_service_test)


  # Antag att 'labelText' är en kolumn i dina sf-objekt som innehåller den text du vill visa i popups
  # Om så inte är fallet, behöver du först skapa den. Exempel:
  apotek_sf$labelText <- "Apotekstjänst"
betalnings_formedling_sf$labelText <- "Betalningsförmedling"
dagkasse_insattning_sf$labelText <- "Dagkasseinsättning"
dagligvaror_EJ_fullsort_sf$labelText <- "Dagligvaror Ej Fullsortiment"
dagligvaror_fullsort_sf$labelText <- "Dagligvaror Fullsortiment"
drivmedel_personbil_sf$labelText <- "Drivmedel för Personbil"
posttjanster_sf$labelText <- "Posttjänster"
uttagsautomat_sf$labelText <- "Uttagautomat"
funktionella_orter_kommersiell_service$labelText <- paste("En funktionell ort, ett försök att inkludera hela Dalarna i analysen, inte bara aktivitet i tätorter",
"Funktionella orter består av tätorter, småorter, fritidshusområden och anläggningsområden", sep = "<br>")
regionalnod_kommersiell_service$labelText <- paste("Regionalnod definition: Kommersiell service samt en annan grundläggande service på platsen.",
                                                   "Med kommersiell service menas dagligvaruhandel och/eller drivmedel.",
                                                   "Med annan grundläggande service menas post-och pakettjänster, kontantuttag, betaltjänster eller",
                                                   "dagskassehantering, samt ombudstjänster för apoteksvaror.", sep = "<br>")

# Visualize


mapview(funktionella_orter_kommersiell_service, col.regions = "grey", label = "labelText", layer.name = "Funktionella Orter", homebutton = FALSE) +
  mapview(regionalnod_kommersiell_service, col.region = "black", cex = 20, alpha.regions = 0.2, label = "labelText", layer.name = "Regionalnoder", homebutton = FALSE) +
  mapview(apotek_sf, col.region = "blue", label = "labelText", layer.name = "Apotek", homebutton = FALSE) +
  mapview(betalnings_formedling_sf, col.region = "red", label = "labelText", layer.name = "Betalningsförmedling", homebutton = FALSE) +
  mapview(dagkasse_insattning_sf, col.region = "green", label = "labelText", layer.name = "Dagkasseinsättning", homebutton = FALSE) +
  mapview(dagligvaror_EJ_fullsort_sf, col.region = "yellow", label = "labelText", layer.name = "Dagligvaror Ej Fullsortiment", homebutton = FALSE) +
  mapview(dagligvaror_fullsort_sf, col.region = "purple", label = "labelText", layer.name = "Dagligvaror Fullsortiment", homebutton = FALSE) +
  mapview(drivmedel_personbil_sf, col.region = "orange", label = "labelText", layer.name = "Drivmedel för Personbil", homebutton = FALSE) +
  mapview(posttjanster_sf, col.region = "brown", label = "labelText", layer.name = "Posttjänster", homebutton = FALSE) +
  mapview(uttagsautomat_sf, col.region = "pink", label = "labelText", layer.name = "Uttagsautomat", homebutton = FALSE)

# # Visa kartan
# map


# ===================== Bredband =============================
bredband_fil <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/Bredband/PTS_brbkart2021_byggn_korr2.gpkg"

st_layers(bredband_fil)

bredband <- st_read(bredband_fil, layer = "PTS_brbkart2021_byggn_korr2", crs = 3006)

bredband <- bredband %>% 
  select(ANDAMAL_1T, uuid, kommunkod, tatortkod_21, smaortkod_15, hus_antal, arb_antal, fritidshus, fiber_absnarhet, fiber)|>
  st_make_valid()

# To determine the percentage of buildings connected to the internet based on a binary variable fiber_tillganglighet 
# (where 1 indicates internet connection and 0 indicates no internet connection), you can follow a similar approach 
# to the script you provided, with adjustments for the specific variable and calculation you're interested in. 

# If necessary, create centroids for buildings data (if they're not already centroids)
buildings_centroids <- st_centroid(bredband)

# Spatial join: match buildings to their respective "funktionella_orter"
buildings_in_orter <- st_join(buildings_centroids, funktionella_orter, join = st_within)

# Calculate the share of buildings with internet connection for each "funktionella_ort"
internet_connection_summary <- buildings_in_orter %>%
  group_by(unique_id) %>%  # Assume 'unique_id' uniquely identifies "funktionella_orter"
  summarise(
    sum_byggnader = n(),
    byggnad_fiber_absnarhet = sum(fiber_absnarhet, na.rm = TRUE),
    andel_homes_passed = (byggnad_fiber_absnarhet / sum_byggnader) * 100
  )

# Convert to a regular dataframe if it's an sf object (optional)
internet_connection_summary_df <- as.data.frame(internet_connection_summary)

# Join this summary back to the "funktionella_orter" spatial data for mapping
funktionella_orter_bredband <- funktionella_orter %>%
  left_join(internet_connection_summary_df, by = "unique_id")%>%
  mutate(andel_homes_passed = paste0(round(andel_homes_passed, 1), "%"))

regionalnod_bredband <- funktionella_orter_bredband %>% 
  st_centroid() %>% 
  filter(andel_homes_passed >=60)

# skapar en ny variabel till pop up i Mapview
regionalnod_bredband$labelText <- paste("Regionalnod bredband definition:", 
                                        "Minst 60 % av byggnader i orten ", 
                                        "har fiber i direkt närhet till fastighet", sep = "<br>") 

# Visualize on a map
mapview(funktionella_orter_bredband)+
  mapview(regionalnod_bredband, cex = 10)

# ===================== Offentlig service (sjukvård) ==========
# Läs in data
sjukvard <- read.csv(sjukvard_fil, header = TRUE, sep = ";", fileEncoding = "ISO-8859-1")
# Ersätt kommatecken med punkter i koordinatkolumnerna
sjukvard$Sweref99X <- as.numeric(gsub(",", ".", sjukvard$Sweref99X))
sjukvard$Sweref99Y <- as.numeric(gsub(",", ".", sjukvard$Sweref99Y))

# Kontrollera igen för att säkerställa att konverteringen lyckades
head(sjukvard)

# Försök konvertera till sf-objekt igen, efter att ha filtrerat bort eventuella NA-värden
sjukvard_filtered <- sjukvard %>%
  filter(!is.na(Sweref99X) & !is.na(Sweref99Y))

sjukvard_sf <- sjukvard_filtered %>%
  st_as_sf(coords = c("Sweref99Y", "Sweref99X"), crs = 3006, agr = "constant")

sjukvard_sf <- sjukvard_sf %>% 
  filter(!is.na(FghKlass))

# Perform spatial join with funktionella_orter
sjukvard_in_orter <- st_join(sjukvard_sf, funktionella_orter, join = st_within)

# Aggregate service information for each funktionell_ort
sjukvard_aggregates <- sjukvard_in_orter %>%
  group_by(unique_id) %>%
  summarise(
    has_VC = any(FghKlass == "Vårdcentral"), # Anta att "VC" står för vårdcentral
    has_SH = any(FghKlass == "Sjukhus"), # Anta att "SJH" står för sjukhus
    sum_sjukvard = n(),
    sum_VC = sum(FghKlass == "Vårdcentral"),
    sum_SH = sum(FghKlass == "Sjukhus"),
    n_unik_sjukvard = n_distinct(FghKlass)
  )

# Join the aggregates back to the funktionella_orter sf object
funktionella_orter_sjukvard <- st_join(funktionella_orter, sjukvard_aggregates, by = "unique_id")

regionalnod_sjukhus <- funktionella_orter_sjukvard %>%
  filter(has_SH == TRUE, has_VC == TRUE) %>% 
  st_centroid()

regionalnod_sjukhus$labelText <- paste("Regional nod Sjukhus definition:","Platser med Sjukhus", sep = "<br>")

regionalnod_vardcentral <- funktionella_orter_sjukvard %>%
  filter(has_VC == TRUE) %>% 
  st_centroid()

regionalnod_vardcentral$labelText <- paste("Regional nod Vårdcentral definition:","Platser med Vårdcentral", sep = "<br>")

mapview(regionalnod_sjukhus, cex = 10)+
  mapview(regionalnod_vardcentral, cex = 6)+
  mapview(funktionella_orter_sjukvard)

# ===================== sysselsättning =================

dagbef <- st_read(dagbef_fil) |> 
  st_transform(3006) |> 
  st_make_valid()

#make centroids
dagbef_centroids <- dagbef |> 
  st_centroid()

sysselsattning <- st_join(dagbef_centroids, funktionella_orter, join = st_within)

# Summarize workforce data for each area
sum_sysselsattning <- sysselsattning %>%
  group_by(unique_id) %>%  # Replace 'area_id' with the identifier for your areas of interest
  summarise(sum_dagbef = sum(dagbef, na.rm = TRUE))

# Convert to a regular dataframe if it's an sf object
sum_sysselsattning_df <- as.data.frame(sum_sysselsattning)

funktionella_orter_sysselsattning <- funktionella_orter %>%
  left_join(sum_sysselsattning_df, by = "unique_id") %>%
  mutate(sum_dagbef = ifelse(is.na(sum_dagbef), NA, sum_dagbef))

# mapview(funktionella_orter_sysselsattning, zcol = "sum_dagbef")


# make centroids of funktionella_orter_sysselsattning filter >= 100 sum_dagbef

funktionella_orter_sysselsattning_centroids <- funktionella_orter_sysselsattning |> 
  st_centroid()

regionalnod_sysselsattning <- funktionella_orter_sysselsattning_centroids |> 
  filter(sum_dagbef >= 100)

regionalnod_sysselsattning$labelText <- paste("Regional nod sysselsättning definition:", 
                                              "Platser med minst 100 arbetstillfällen.", 
                                              "Oberoende av privat eller offentlig sektor.",
                                              "Oberoende av bransch.", sep = "<br>")

# mapview(regionalnod_sysselsattning, zcol = "sum_dagbef", cex = "sum_dagbef")+
#   mapview(tatort_layer, col.regions = "blue", alpha.regions = 0.3)
# 
# st_write(funktionella_orter_sysselsattning, "funktionella_orter.gpkg", layer = "funktionella_orter_sysselsattning", driver = "GPKG")



funktionella_orter_joined <- st_join(funktionella_orter_bredband, funktionella_orter_kommersiell_service, join_by = "unique_id") %>% 
  st_join(funktionella_orter_sysselsattning) %>% 
  st_join(funktionella_orter_sjukvard)

funktionella_orter_final <- funktionella_orter_joined %>%
  select(funk_ort = funk_ort.x, omr = omr.x, kod = kod.x, namn_ort = namn_ort.x, kommunnamn = kommunnamn.x,
         kommun = kommun.x, befolkning = befolkning.x, priority = priority.x, unique_id = unique_id.x.x,
         sum_byggnader, byggnad_fiber_absnarhet, andel_homes_passed, n_service,
         sum_grund_serv, sum_kom_serv, n_unik_grund_kom_serv, n_unik_alla_typer, labelText, sum_dagbef, 
         sum_sjukvard, sum_VC, sum_SH, n_unik_sjukvard, geom = geom.x)


mapview(funktionella_orter_final, col.regions = "lightblue", alpha.regions = 0.2, label = "labelText")+
  mapview(regionalnod_sjukhus, col.regions = "red", cex = 15, label = "labelText")+
  mapview(regionalnod_vardcentral, col.regions = "pink", cex = 12, label = "labelText")+
  mapview(regionalnod_kommersiell_service, col.regions = "blue", cex = 9, label = "labelText")+
  mapview(regionalnod_bredband, col.regions = "green", cex = 6, label = "labelText")+
  mapview(regionalnod_sysselsattning, col.regions = "orange", cex = 3, label = "labelText")
