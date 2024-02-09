#Ett skript som skapar regional noder
#Allt snyggt och imponerande nedan är skapat av min bästa kompis GPT4
# 
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
# Om vi vill använda andra orter som t.ex. Tätorter ändra unique_id <- tatortskod

omr_fil <- "G:/Samhällsanalys/GIS/grundkartor/" # filsökväg till småort, tätort och fritidshusområde

smaort <- "smaorter/So2015_Swe99TM.shp"
tatort <- "tatorter/Tatorter_1980_2020.gpkg"
fritid_omr <- "fritidshusomraden/Fo2015_Swe99TM.shp"

fil_anl <- "G:/Samhällsanalys/GIS/Geotorget/anlaggningsomrade_ln20/"  # filsökväg till anläggningsområden
anlag_omr <- "anlaggningsomrade_ln20.gpkg"
#funktionella_orter_fil <- "funktionella_orter.gpkg"                  # gpkg sparat i projektet finns att ladda ner från repository
buffer = 200   # buffer till funktionella orter

# Komersiell service nedladdat från Pipos Serviceanalys 2023-03, 8 st CSV-filer
kom_serv_filer <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/FrånPipos/"

grans_kom_service <- 1
grans_grund_service <- 1

# Bredband, data från Bredbandskoordinator 2023
bredband_fil <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/Bredband/PTS_brbkart2021_byggn_korr2.gpkg"
grans_bredband <- 60 # hur stor andel byggnader är uppkopplade enligt definitionen "homes passed"?
grans_byggnad <- 100 # minsta antal byggnader i en funktionell ort
# Offentlig service (sjukvård, Utbudspunkter)
sjukvard_fil <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/Offentlig_service/Utbudspunkter.csv"

# Sysselsättning
dagbef_fil <- "data/dag_natt_bef_ruta1km.gpkg"               # ersätt med 100 meterruta

grans_dagbef <- 100 



st_layers("data/funktionella_orter.gpkg")

funktionella_orter <- st_read("data/funktionella_orter.gpkg", layer = "funktionella_orter", crs = 3006) 
# ============= Analys Komersiell service =============================================================

# Definition: Kommersiell service samt en annan grundläggande service på platsen.   
# Med kommersiell service menas dagligvaruhandel och/eller drivmedel.  
# Med annan grundläggande service menas post-och pakettjänster, kontantuttag, betaltjänster eller  
# dagskassehantering, samt ombudstjänster för apoteksvaror.

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

# Join the aggregates back to the funktionella_orter sf object
funktionella_orter_kommersiell_service <- st_join(funktionella_orter, service_aggregates, by = "unique_id")

# Filtrera för att hitta orter med minst en grundläggande och en kommersiell service
regionalnod_kommersiell_service <- funktionella_orter_kommersiell_service %>%
  filter(sum_grund_serv >= grans_grund_service & sum_kom_serv >= grans_kom_service)

# Beräkna centroider för dessa filtrerade orter
regionalnod_kommersiell_service <- st_centroid(regionalnod_kommersiell_service)


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
funktionella_orter_kommersiell_service$labelText <- paste("Detta är en funktionell ort. Ett experiment och ett försök att inkludera hela Dalarna, ",
                                                          "istället för att endast analysera aktivitet i tätorter. Funktionella orter består av tätorter, ",
                                                          " småorter, fritidshusområden och anläggningsområden", sep = "<br>")
regionalnod_kommersiell_service$labelText <- paste("Regionalnod definition: Kommersiell service samt en annan grundläggande service på platsen.",
                                                   "Med kommersiell service menas dagligvaruhandel och/eller drivmedel.",
                                                   "Med annan grundläggande service menas post-och pakettjänster, kontantuttag, betaltjänster eller",
                                                   "dagskassehantering, samt ombudstjänster för apoteksvaror.", sep = "<br>")

# mapview(funktionella_orter_kommersiell_service, col.regions = "grey", label = "labelText", layer.name = "Funktionella Orter", homebutton = FALSE) +
#   mapview(regionalnod_kommersiell_service, col.regions = "black", cex = 20, alpha.regions = 0.2, label = "labelText", layer.name = "Regionalnoder komersiell service", homebutton = FALSE) +
#   mapview(apotek_sf, col.regions = "blue", label = "labelText", layer.name = "Apotek", homebutton = FALSE) +
#   mapview(betalnings_formedling_sf, col.regions = "red", label = "labelText", layer.name = "Betalningsformedling", homebutton = FALSE) +
#   mapview(dagkasse_insattning_sf, col.regions = "green", label = "labelText", layer.name = "Dagkasseinsattning", homebutton = FALSE) +
#   mapview(dagligvaror_EJ_fullsort_sf, col.regions = "yellow", label = "labelText", layer.name = "Dagligvaror Ej Fullsortiment", homebutton = FALSE) +
#   mapview(dagligvaror_fullsort_sf, col.regions = "purple", label = "labelText", layer.name = "Dagligvaror Fullsortiment", homebutton = FALSE) +
#   mapview(drivmedel_personbil_sf, col.regions = "orange", label = "labelText", layer.name = "Drivmedel Personbil", homebutton = FALSE) +
#   mapview(posttjanster_sf, col.regions = "brown", label = "labelText", layer.name = "Posttjanster", homebutton = FALSE) +
#   mapview(uttagsautomat_sf, col.regions = "pink", label = "labelText", layer.name = "Uttagsautomat", homebutton = FALSE)

# ===================== Bredband =============================


bredband_fil <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/Bredband/PTS_brbkart2021_byggn_korr2.gpkg"

st_layers(bredband_fil)

bredband <- st_read(bredband_fil, layer = "PTS_brbkart2021_byggn_korr2", crs = 3006)

bredband <- bredband %>% 
  select(ANDAMAL_1T, uuid, kommunkod, tatortkod_21, smaortkod_15, hus_antal, arb_antal, fritidshus, fiber_absnarhet, fiber)|>
  st_make_valid()

# mapview(bredband, zcol = "fiber_absnarhet")

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

# regional_nod skapas längre ner med funktionella_orter_final
# regionalnod_bredband <- funktionella_orter_bredband %>% 
#   st_centroid() %>% 
#   filter(andel_homes_passed >=60)

# ===================== Offentlig service (sjukvård) ==========
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
# skapa separata sf_objekt från kolumnen FghKlass
NULL_sf <- sjukvard_sf %>% 
  filter(FghKlass == "NULL")

blank_sf <- sjukvard_sf %>% 
  filter(FghKlass == "")

vardcentral_sf <- sjukvard_sf %>% 
  filter(FghKlass == "Vårdcentral")

sjukhus_sf <- sjukvard_sf %>% 
  filter(FghKlass == "Sjukhus")

mapview(blank_sf, col.regions = "darkred", hide = TRUE)+
  mapview(NULL_sf, col.regions = "red", hide = TRUE)+
  mapview(vardcentral_sf, col.regions = "green")+
  mapview(sjukhus_sf, col.regions = "darkgreen")
  

sjukvard_sf <- sjukvard_sf %>% 
  filter(!is.na(FghKlass))

sjukvard_sf <- sjukvard_sf %>% 
  filter(FghKlass != "NULL")

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

mapview(regionalnod_sjukhus, col.regions = "darkgreen", cex = 15, alpha.regions = 0.7, homebutton = FALSE, lwd = 0.1, hide = TRUE)+
     mapview(regionalnod_vardcentral, col.regions = "chartreuse1", cex = 10, alpha.regions = 0.7, homebutton = FALSE, lwd = 0.1, hide = TRUE)+
  mapview(sjukvard_sf, zcol = "FghKlass", label = "FghKlass", homebutton = FALSE)
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

# make centroids of funktionella_orter_sysselsattning filter >= 100 sum_dagbef

funktionella_orter_sysselsattning_centroids <- funktionella_orter_sysselsattning |> 
  st_centroid()

regionalnod_sysselsattning <- funktionella_orter_sysselsattning_centroids |> 
  filter(sum_dagbef >= grans_dagbef)

regionalnod_sysselsattning$labelText <- paste("Regional nod sysselsättning definition:", 
                                              "Platser med minst 100 arbetstillfällen.", 
                                              "Oberoende av privat eller offentlig sektor.",
                                              "Oberoende av bransch.", sep = "<br>")


# ========================== Kartan =================================================
center <- st_coordinates(st_centroid(st_union(funktionella_orter_final)))[1, ]
zoom <- 20

# mapview(funktionella_orter_final, center = center, zoom = zoom)
# 
# mapview(funktionella_orter_final, col.regions = "lightblue", alpha.regions = 0.2, label = "labelText", homebutton = FALSE, layer.name = "Funktionella orter", lwd = 1)+
#   mapview(regionalnod_sjukhus, col.regions = "chartreuse4", cex = 25, label = "labelText", alpha.regions = 0.7, homebutton = FALSE, lwd = 0.1)+
#   mapview(regionalnod_vardcentral, col.regions = "chartreuse1", cex = 20, label = "labelText", alpha.regions = 0.7, homebutton = FALSE, lwd = 0.1)+
#   mapview(regionalnod_kommersiell_service, col.regions = "gold1", cex = 15, label = "labelText", alpha.regions = 0.7, homebutton = FALSE, lwd = 0.1)+
#   mapview(regionalnod_bredband, col.regions = "orange2", cex = 10, label = "labelText", alpha.regions = 0.7, homebutton = FALSE, lwd = 0.1)+
#   mapview(regionalnod_sysselsattning, col.regions = "red4", cex = 5, label = "labelText", alpha.regions = 0.7, homebutton = FALSE, lwd = 0.1)+
#   mapview(apotek_sf, col.regions = "blueviolet", label = "labelText", layer.name = "Apotek", homebutton = FALSE, hide = TRUE) +
#   mapview(betalnings_formedling_sf, col.regions = "darkorchid1", label = "labelText", layer.name = "Betalningsformedling", homebutton = FALSE, hide = TRUE) +
#   mapview(dagkasse_insattning_sf, col.regions = "lightsalmon2", label = "labelText", layer.name = "Dagkasseinsattning", homebutton = FALSE, hide = TRUE) +
#   mapview(dagligvaror_EJ_fullsort_sf, col.regions = "deeppink4", label = "labelText", layer.name = "Dagligvaror Ej Fullsortiment", homebutton = FALSE, hide = TRUE) +
#   mapview(dagligvaror_fullsort_sf, col.regions = "lightslateblue", label = "labelText", layer.name = "Dagligvaror Fullsortiment", homebutton = FALSE, hide = TRUE) +
#   mapview(drivmedel_personbil_sf, col.regions = "deeppink", label = "labelText", layer.name = "Drivmedel Personbil", homebutton = FALSE, hide = TRUE) +
#   mapview(posttjanster_sf, col.regions = "brown", label = "labelText", layer.name = "Posttjanster", homebutton = FALSE, hide = TRUE) +
#   mapview(uttagsautomat_sf, col.regions = "pink", label = "labelText", layer.name = "Uttagsautomat", homebutton = FALSE, hide = TRUE)+
#   mapview(smaort_layer, col.regions = "cyan2", hide = TRUE, homebutton = FALSE, layer.name = "Smaorter") + 
#   mapview(tatort_layer, col.regions = "cyan4", hide = TRUE, homebutton = FALSE, layer.name = "Tatorter") + 
#   mapview(fritid_layer, col.regions = "cyan1", hide = TRUE, homebutton = FALSE, layer.name = "Fritidshusomrade") + 
#   mapview(anlag_layer, col.regions = "cyan3", hide = TRUE, homebutton = FALSE, layer.name = "Anlaggningsomrade")


# Write the data to a GPKG file
#st_write(funktionella_orter_final, "funk_ort_final.gpkg", "funktionella_orter")

# regional nod skola, OBS!! data insamlat av praktikant manuellt med google och eniro

offentlig_service_sokvag <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/Offentlig_service/hanna_praktikant/"
offentlig_service_fil <- "offentlig_service.csv"

offentlig_service <- read.csv(paste0(offentlig_service_sokvag, offentlig_service_fil), header = TRUE, sep = ";")

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
bibliotek_sf_object <- list_sf_objects[["Bibliotek"]]
grundskola_sf_object <- list_sf_objects[["Grundskola"]]
polis_sf_object <- list_sf_objects[["Polis"]]
brandkar_sf_object <- list_sf_objects[["Brandkår"]]
gymnasieskola_sf_object <- list_sf_objects[["Gymnasieskola"]]
fritidsgard_sf_object <- list_sf_objects[["Fritidsgård"]]
vardcentral_sf_object <- list_sf_objects[["Vårdcentral"]]
lakarmottagning_sf_object <- list_sf_objects[["Läkarmottagning"]]
tandlakare_sf_object <- list_sf_objects[["Tandläkare"]]
hogskola_sf_object <- list_sf_objects[["högskola"]]

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
# mapview(all_schools_in_orter, zcol = "typ_service")+
#   mapview(funktionella_orter)

# Städa bort onödiga kolumner, om det behövs
all_schools_in_orter <- all_schools_in_orter %>% 
  select(-c(ovriga_kolumner_om_det_behovs))  # Anpassa för faktiska kolumner

# Aggregera skolinformation för varje funktionell ort
school_aggregates <- all_schools_in_orter %>%
  group_by(unique_id) %>%
  summarise(
    n_skolor = n(),
    sum_grundskolor = sum(typ_service == "Grundskola"),
    sum_gymnasieskolor = sum(typ_service == "Gymnasieskola"),
    sum_hogskolor = sum(typ_service == "högskola"),  # Lägg till högskola
    n_unik_skoltyp = n_distinct(typ_service)
  )
# Join the aggregates back to the funktionella_orter sf object
funktionella_orter_skola <- st_join(funktionella_orter, school_aggregates, by = "unique_id")
# mapview(funktionella_orter_skola)+
#   mapview(all_schools_sf, zcol = "typ_service")


# =================  join av funktionella orter ========================

perform_join <- function(x, y, suffixes = c(".x", ".y")) {
  joined <- st_join(x, y)
  # Automatically make column names unique
  colnames(joined) <- make.unique(colnames(joined))
  return(joined)
}


funktionella_orter_joined <- funktionella_orter_bredband %>%
  perform_join(funktionella_orter_kommersiell_service) %>%
  perform_join(funktionella_orter_sysselsattning) %>%
  perform_join(funktionella_orter_sjukvard) %>% 
  perform_join(funktionella_orter_skola)
# names(funktionella_orter_final)

funktionella_orter_final <- funktionella_orter_joined %>%
  select(Typ_av_omr = omr.x...3, kod = kod.x...4, namn_ort = namn_ort.x...5, kommunnamn = kommunnamn.x...6,
         kommun = kommun.x...7, befolkning = befolkning.x...8,
         sum_byggnader, byggnad_fiber_absnarhet, andel_homes_passed, n_service,
         sum_grund_service = sum_grund_serv, sum_kom_service = sum_kom_serv, n_unik_grund_kom_service = n_unik_grund_kom_serv, n_unik_alla_typer_service = n_unik_alla_typer, labelText, sum_dagbef, 
         sum_utbud = sum_sjukvard, sum_vardcentral = sum_VC, sum_sjukhus = sum_SH, n_unik_sjukvard, n_skolor, sum_grundskolor, sum_gymnasieskolor, sum_hogskolor, n_unik_skoltyp, geom = geom.x)

funktionella_orter_final$labelText <- paste("Detta är en funktionell ort. Ett experiment och ett försök att inkludera hela Dalarna, ",
                                            "istället för att endast analysera aktivitet i tätorter. Funktionella orter består av tätorter, ",
                                            " småorter, fritidshusområden och anläggningsområden", sep = "<br>")

regionalnod_bredband <- funktionella_orter_final %>%
  st_centroid() %>% 
  filter(andel_homes_passed >= grans_bredband) %>% 
  filter(sum_byggnader >= grans_byggnad) %>% 
  select(Typ_av_omr, kod, namn_ort, 
         kommunnamn, kommun, befolkning, 
         sum_byggnader, byggnad_fiber_absnarhet, 
         andel_homes_passed)


# skapar en ny variabel till pop up i Mapview
regionalnod_bredband$labelText <- paste("Regionalnod bredband definition:", 
                                        "Minst 60 % av byggnader i orten ", 
                                        "har fiber i direkt närhet till fastighet", sep = "<br>") 
