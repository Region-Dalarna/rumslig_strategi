library(keyring)
library(httr)
library(tidyverse)
library(readxl)
library(openxlsx)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R", encoding = "utf-8", echo = FALSE)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_postgis.R", encoding = "utf-8", echo = FALSE)
#source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_filer.R", encoding = "utf-8", echo = FALSE)

# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, 
                     username = key_list(service = "auth")$username, password = key_get("auth", key_list(service = "auth")$username)))
set_config(config(ssl_verifypeer = 0L))

# avoid scientific notation
options(scipen=999)

# ========================================== inställningar ===============================================

data_input <- "G:/Samhällsanalys/GIS/projekt/gtfs"                     # mapp för gtfs-filer

# todays date, used as filter
#datum_filter <- str_remove_all(Sys.Date(), "-")                      # det datum vi vill använda (idag om man inte ändrar)

# # datum en hel vecka från idag
# datum_filter <- str_remove_all(as.Date(Sys.Date()+0:6), "-")

ar_vardag <- "20240123"                         # vi väljer en vardag för att filtrera på vardagar nedan
ar_helg <- c("20240120", "20240121")            # vi väljer en lördag och söndag för att filtrera på helger nedan

# ange operatör
rkm <- "dt" # !!!!!! Specify RKM. Available values : sl, ul, sormland, otraf, krono, klt, gotland, blekinge, skane, halland, vt, varm, orebro, vl, dt, xt, dintur, sj

# ange länskod
lan_kod <- "20" # !!!!!! Specify län kod, Uppsala = 03, Dalarna = 20


# ============================== ladda ner gtfs från Trafiklab, Samtrafiken =================================


gtfs_regional_fil <- paste0(data_input, "/trafiklab_", rkm, ".zip")            # sätt ihop filnamn för gtfs-filen

## static GTFS timetable data from Trafiklab
url_regional <- paste0("https://opendata.samtrafiken.se/gtfs/", rkm, "/", rkm, ".zip?key=", key_get("API_trafiklab_token", "GTFS_Regional"))

GET(url_regional, write_disk(gtfs_regional_fil, overwrite=TRUE))           # här laddar vi ned filen

# ============================ läs in gtfs-data till gtfs-routerpaketet =====================================

# läs vilka filer som ingår i gtfs-zipfilen
gtfs_regional_filer <- unzip(zipfile = gtfs_regional_fil, list = TRUE)$Name 

# läs filerna i zip-filen och lägg varje fil som en dataframe i en lista
gtfs_reg_list <- map(gtfs_regional_filer, ~read_csv(unzip(gtfs_regional_fil, .))) # läs in filerna

# döp varje element i listan från filnamnet, men vi tar bort ".txt" i namnet
names(gtfs_reg_list) <- gtfs_regional_filer %>% str_replace(".txt", "")

# =========== ta ut ett datum (datum_filter) och sätt ihop en dataframe med alla linjer och turer ==================

## service_id för det datum vi valt
#service_id_inklud_reg <- gtfs_reg_list$calendar_dates %>% filter(date %in% datum_filter) %>% select(service_id) %>% pull()

# service_id för vald vardag (som definieras i ar_vardag-vektorn)
service_id_vardag <- gtfs_reg_list$calendar_dates %>% filter(date %in% ar_vardag) %>% select(service_id) %>% pull()
# service_id för vald helg (som definieras i ar_helg-vektorn)
service_id_helg <- gtfs_reg_list$calendar_dates %>% filter(date %in% ar_helg) %>% select(service_id) %>% pull()

# # trips (dvs. turer) för det datum vi valt
# trips_inklud_reg <- gtfs_reg_list$trips %>% filter(service_id %in% service_id_inklud_reg) %>% select(trip_id) %>% pull()

# trips (dvs. turer) för den vardag som vi valt
trips_vardag <- gtfs_reg_list$trips %>% filter(service_id %in% service_id_vardag) %>% select(trip_id) %>% pull()
trips_helg <- gtfs_reg_list$trips %>% filter(service_id %in% service_id_helg) %>% select(trip_id) %>% pull()

# här kopplar vi ihop relevant information från alla dataseten i gtfs-zipfilen
gtfs_reg_df <- gtfs_reg_list$stop_times %>%
  left_join(gtfs_reg_list$trips, by = "trip_id") %>%
  left_join(gtfs_reg_list$stops, by = "stop_id") %>%
  left_join(gtfs_reg_list$routes, by = "route_id") %>%
  mutate(hpl_id = substr(stop_id, 8, 13), 
         vardag = ifelse(trip_id %in% trips_vardag, 1, 0),                     # om turen är på de(n) vardag(ar) vi valt
         helg = ifelse(trip_id %in% trips_helg, 1, 0)) %>%                     # om turen är på de(n) helgdag(ar) vi valt
  distinct(arrival_time, departure_time, stop_id, .keep_all= TRUE) # remove duplicates

# Tidtabelldata är på hållplatslägesnivå. Ta medel för att skapa en koordinat per hållplats
# skapa också variabler för om turerna går efter kl 18 på vardagar och om turer går på helgen
gtfs_reg_df2 <- gtfs_reg_df %>% 
  group_by(hpl_id, stop_name) %>% 
  mutate(hpl_lat = round(mean(as.numeric(stop_lat)),5), hpl_lon = round(mean(as.numeric(stop_lon)), 5)) %>% 
  ungroup() %>% 
  select(route_short_name, stop_name, arrival_time, departure_time, stop_sequence, stop_headsign, shape_dist_traveled, direction_id,
         hpl_lat, hpl_lon, route_id, trip_id, stop_id, hpl_id, vardag, helg) %>% 
  mutate(vardag_efter_kl18 = ifelse(departure_time - hms::as_hms("17:59:59") > 0 & vardag == 1, 1, 0),
         helger = ifelse(helg == 1, 1, 0))


# ==================================== hämta och koppla på tätorter ==========================================

tatorter <- hamta_karta(karttyp = "tätorter", regionkoder = "20") %>%       # hämta tätorter från rd-geodatabasen 
  select(c(tatortskod, tatort, kommunkod, kommun, ar, geom))

# aggregera antal avgångar per hållplats och riktning, ta med totalt antal avgångar, 
# avgångar vardagar efter kl. 18 samt avgångar helger
avgangar_hpl <- gtfs_reg_df2 %>% 
  group_by(stop_name, hpl_id, direction_id,  hpl_lat, hpl_lon) %>% 
  summarise(antal_avg = n(),
            antal_avg_efter18 = sum(vardag_efter_kl18),
            antal_avg_helg = sum(helger)) %>% 
  ungroup()

# # lägg på totalt antal avgångar per hållplats (dvs. summerat för båda riktningar)
# avgangar_hpl <- avgangar_hpl %>% 
#   group_by(stop_name, hpl_id, hpl_lat, hpl_lon) %>% 
#   mutate(antal_avg_tot = sum(antal_avg)) %>% 
#   ungroup()

# därefter beräknar vi vilken hållplats som har flest avgångar per tätort och behåller bara den så att vi får
# en hållplats per tätort (den med flest avgångar)
hpl_tatorter_alla <- avgangar_hpl %>%
  st_as_sf(coords = c("hpl_lon", "hpl_lat"), crs = 4326) %>%              # läs in koordinater från gtfs
  st_transform(crs = 3006) %>%                                            # läs in koordinater från gtfs
  st_join(tatorter) %>%                                                   # lägg ihop med tätortslagret som vi läste in ovan
  filter(!is.na(tatort)) %>%                                              # ta bort hållplatser utanför tätorter
  st_drop_geometry() %>%                                                  # nu är vi klara med geom så vi släpper det
  select(tatortskod, tatort, hpl_id, stop_name, direction_id,             # välj ut relevanta kolumner och lägg i rätt 
         antal_avg, antal_avg_efter18, antal_avg_helg)     # ordning
  
# sen aggregerar vi per hållplats och rikting, räknar antal turer i varje riktning totalt samt även 
# vardagar efter kl. 18 och helger
hpl_tatort_riktn_aggr <- hpl_tatorter_alla %>% 
  group_by(tatortskod, tatort, direction_id) %>% 
  summarise(hpl_max_avg = max(antal_avg, na.rm = TRUE),
            hpl_max_avg_efter18 = max(antal_avg_efter18, na.rm = TRUE),
            hpl_max_avg_helg = max(antal_avg_helg, na.rm = TRUE),
            minst_sju_turer = ifelse(hpl_max_avg >= 7, 1, 0)) %>% 
  ungroup()

# därefter aggregerar vi ytterligare per hållplats utan riktning som vi räknat ut ovan

hpl_tatort_aggr <- hpl_tatort_riktn_aggr %>%
  group_by(tatortskod, tatort) %>% 
  summarise(minst_sju_dubbelturer = ifelse(sum(minst_sju_turer, na.rm = TRUE) > 1, 1, 0),
            enkelturer_fler_an_14 = ifelse(n_distinct(direction_id) == 1 & 
                                             sum(hpl_max_avg, na.rm = TRUE) >= 14, 1, 0),
            tur_efter18 = ifelse(sum(hpl_max_avg_efter18, na.rm = TRUE) > 0, 1, 0),
            tur_helg = ifelse(sum(hpl_max_avg_helg, na.rm = TRUE) > 0, 1, 0)) %>% 
  ungroup()

# OBS! Hur tänker vi kring ex. Sollerön. Har 15 turer per dag, alla går åt samma håll
# Är det någon form av "ring"-trafik där? Dvs. att alla bussar går åt samma håll i en 
# form av ringlinje. Dvs. även dubbelturer går åt samma håll. För att ha lite koll på dem
# så har en variabel lagts till där vi ser om man bara har turer åt ena riktningen men 
# fler än 14 turer per vecka.

# spara som excelfil
write.xlsx(hpl_tatort_aggr, paste0(data_input, "/tatorter_noder.xlsx"), overwrite = TRUE)



# här nedan är lite testgrejer

# ================== skapa df med service-id och vilka dagar de kör ======================================
# ett test för att skapa en df med bra överblick över vilka dagar som olika service-id kör, behövs kanske inte

original_kal_df <- gtfs_reg_list$calendar_dates

cal_dates_df <- gtfs_reg_list$calendar_dates %>% 
  mutate(service_id = service_id %>% as.integer(),
         datum = as.Date(paste0(str_sub(date, 1,4), "-",
                                str_sub(date, 5,6), "-",
                                str_sub(date, 7,8))),
         veckodag = weekdays(datum),
         weekday = c("Sunday", "Monday", "Tuesday",     # Convert dates to weekdays
                     "Wednesday", "Thursday", "Friday",
                     "Saturday")[as.numeric(format(datum, "%w"))+1]) %>% 
  pivot_wider(names_from = weekday, values_from = exception_type) %>%
  replace(is.na(.), 0) %>%
  mutate(service_id = service_id %>% as.integer()) %>% 
  group_by(service_id) %>% 
  summarise(monday = max(Monday),
            tuesday = max(Tuesday),
            wednesday = max(Wednesday),
            thursday = max(Thursday),
            friday = max(Friday),
            saturday = max(Saturday),
            sunday = max(Sunday),
            start_date = min(date),
            end_date = max(date),
            antal = n()) %>% 
  ungroup()

cal_dates_df2 <- cal_dates_df %>% 
  rowwise() %>% 
  mutate(vardag = ifelse(sum(monday, tuesday, wednesday, thursday, friday) > 0, 1, 0),
         helg = ifelse(sum(saturday, sunday) > 0, 1, 0))

# antal turer per service_id
service_id_df <- gtfs_reg_df %>% group_by(service_id) %>% summarise(antal = n())

