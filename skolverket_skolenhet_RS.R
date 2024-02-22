library(httr)
library(keyring)
library(jsonlite)
library(tidyverse)
library(data.table)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

# Skolenheter
resp <- GET("https://api.skolverket.se/skolenhetsregistret/v1/skolenhet")
text_api <- fromJSON(content(resp, type = "text", encoding = "UTF-8"), flatten = TRUE)
skolenheter_df <- text_api$Skolenheter %>% 
  filter(Status == "Aktiv")

skolor_dalarna <- text_api$Skolenheter %>% 
  filter(Status == "Aktiv",
         str_sub(Kommunkod, 1, 2) %in% "20")

# här hämtar vi hem en lista med skolenheter + information om dem
skolor_dalarna_info <- map(skolor_dalarna$Skolenhetskod, function(kod) {
  resp <- GET(paste0("https://api.skolverket.se/skolenhetsregistret/v1/skolenhet/", kod))
  text_api <- fromJSON(content(resp, type = "text", encoding = "UTF-8"), flatten = TRUE)
}, .progress = TRUE)

las_post <- function(list_element) {
  res <- unlist(list_element)
  #names(res)

    res_df <- data.frame(
         Namn = res['SkolenhetInfo.Namn'],
         Skolenhetskod = res['SkolenhetInfo.Skolenhetskod'],
         Adress = res['SkolenhetInfo.Besoksadress.Adress'],
         Postnr = res['SkolenhetInfo.Besoksadress.Postnr'],
         Ort = res['SkolenhetInfo.Besoksadress.Ort'],
         Koord_x = gsub(",", ".", res['SkolenhetInfo.Besoksadress.GeoData.Koordinat_SweRef_E']), # byt ut , mot . för att få numeriskt värde
         Korrd_y = gsub(",", ".", res['SkolenhetInfo.Besoksadress.GeoData.Koordinat_SweRef_N']), # byt ut , mot . för att få numeriskt värde
         Punkttyp = gsub(",", ".", res['SkolenhetInfo.Besoksadress.GeoData.Koordinat_SweRef_N']), # byt ut , mot . för att få numeriskt värde
         Skolformer_typ = res[str_detect(names(res), 'SkolenhetInfo.Skolformer.type')],                              # är NULL men funkar att extrahera ändå
         Skolformer_benamning = res[str_detect(names(res), 'SkolenhetInfo.Skolformer.Benamning')],
         Skolformer_id = res['SkolenhetInfo.Skolformer.SkolformID'],
         Kommunkod = res['SkolenhetInfo.Kommun.Kommunkod'],
         Kommun = res['SkolenhetInfo.Kommun.Namn'],
         Huvudman_namn = res['SkolenhetInfo.Huvudman.Namn'],
         Huvudman_typ = res['SkolenhetInfo.Huvudman.Typ'],
         Status = res['SkolenhetInfo.Status']
       )
  
    rownames(res_df) <- NULL
    return(res_df)

} # slut las_post

skolor_dalarna_geo <- map(skolor_dalarna_info, ~ las_post(.x), .progress = TRUE) %>% list_rbind()

# för att se vilka olika skolformer vi har i datasetet
unique(skolor_dalarna_geo$Skolformer_benamning)

# # bara SFI på Komvux
# Komvux_SFI <- skolor_dalarna_geo %>% 
#   filter(Skolformer_typ == "Sfi")
# 
# # bara Komvux 
# Komvux <- skolor_dalarna_geo %>% 
#   filter(Skolformer_typ == "Komvux")
# 
# # bara Grundskola
# grundskola_Dalarna <- skolor_dalarna_geo %>% 
#   filter(Skolformer_typ == "Grundskola")

# ta ut grundskolor som saknar koordinater
grundskola_saknar_koord <- skolor_dalarna_geo %>% 
  filter(is.na(Koord_x))

# ======================================================== koppla på koordinater från adressregistret ===================================================
# fil med adresser från Lantmäteriet
inputmapp <- "G:/Samhällsanalys/GIS/adresser/datafiler/"
adress_lm_fil <- paste0(inputmapp, "ADRPL_90A_dalarna_small.txt")     # filnamn adressregistret från Lantmäteriet

# Läs in adresser och filtrera ut Dalarna
adresser_dalarna <- fread(adress_lm_fil, colClasses = "character", encoding = "Latin-1") %>%   
  mutate(kommunkod = kommunkod %>% str_pad(width = 4, side = "left", pad = "0") %>% as.character(),
         lanskod = str_sub(kommunkod, 1, 2) %>% as.character()) %>% 
  filter(lanskod == "20") %>% 
  mutate(adrplats_num = ADRPLATS %>% parse_number() %>% as.character(),
         adrplats_bokstav = str_remove(ADRPLATS, adrplats_num),
         adrplats_num = ifelse(is.na(adrplats_num), "", adrplats_num),
         adrplats_bokstav = ifelse(is.na(adrplats_bokstav), "", adrplats_bokstav),
         adress = paste0(ADROMRADE, " ", adrplats_num, " ", adrplats_bokstav) %>% str_trim() %>% tolower(),         # skapa adress att koppla mot
         POSTNR = POSTNR %>% as.character(),
         adress_med_gardsnamn = paste(ADROMRADE, GARDSNAMN, adrplats_num, adrplats_bokstav) %>% str_squish() %>% tolower())

# alla adresser - koda kolumn "ej_join" och sätt 1 för de som har fått koordinater och 0 för de som inte fått det
adress_join <- grundskola_saknar_koord %>% 
  mutate(adress_join = Adress %>% tolower()) %>% 
  left_join(adresser_dalarna, by = c("adress_join" = "adress_med_gardsnamn", "Postnr" = "POSTNR"), relationship = "many-to-one") %>% 
  mutate(ej_join = ifelse(is.na(XKOORD), 1, 0)) %>% 
  rename(x_koord = YKOORD,
         y_koord = XKOORD)

# ========================================================================================================================================================

grundskola_Dalarna2 <- skolor_dalarna_geo %>% 
  left_join(adress_join %>% select(Skolenhetskod, x_koord, y_koord), by = "Skolenhetskod") %>% 
  mutate(Koord_x = ifelse(is.na(Koord_x), x_koord, Koord_x),
         Korrd_y = ifelse(is.na(Korrd_y), y_koord, Korrd_y)) %>% 
  select(-c(x_koord, y_koord))

unique(grundskola_Dalarna2$Skolformer_benamning)
  
  # skapa sf objekt och visa i mapview

# 

grundskola_Dalarna2 <- grundskola_Dalarna2 %>% 
  mutate(Koord_x = as.numeric(Koord_x),
         Korrd_y = as.numeric(Korrd_y))

grundskola_Dalarna2 <- st_as_sf(grundskola_Dalarna2, coords = c("Koord_x", "Korrd_y"), crs = 3006)

# Remove rows with NA in coordinates
grundskola_Dalarna2_clean <- grundskola_Dalarna2 %>%
  filter(!is.na(Koord_x) & !is.na(Korrd_y))

# Convert to sf object
grundskola_Dalarna2_sf <- st_as_sf(grundskola_Dalarna2_clean, coords = c("Koord_x", "Korrd_y"), crs = 3006)

mapview(grundskola_Dalarna2_sf, zcol = "Skolformer_benamning")

# st_write(funktionella_orter, "funktionella_orter.gpkg", layer = "funktionella_orter", driver = "GPKG")
st_write(grundskola_Dalarna2_sf, "skola_dalarna.gpkg", layer = "skola_dalarna", driver = "GPKG")



