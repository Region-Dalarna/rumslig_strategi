
# arbeta mer med funktionella orter
# testa med hexagoner!!!

# indikator för publika laddstationer
# ta ut ÅDT per ort, medel, median
# E 16, 70, E45, 50, 68, 66
# dela per anslutningspunkt

if (!require("pacman")) install.packages("pacman")
p_load(httr,
       jsonlite,
       mapview,
       tidyverse,
       tidyr,
       sf,
       tmap,
       dplyr,
       leaflet,
       ggpubr, 
       keyring,
       RPostgreSQL,
       RPostgres)

#Hämta punktlagret med publika laddstationer från Nobil
##
###Hämta din personliga api-nyckel från Nobil via Energimyndigheten 
####https://www.energimyndigheten.se/klimat--miljo/transporter/laddinfrastruktur/registrera-din-laddstation/utvecklare/

#Skapa variabler för webbadress, api-nyckel och diverse. 

# url <- "https://nobil.no/api/server/datadump.php?apikey="
# api_key <- "<din_api_nyckel>"            #ta bort < och > lägg till din apinyckel således med citattecken runt "din_api_nyckel"
div <- "&countrycode=SWE&fromdate=2012-06-02&format=json&file=false"

##Eller lägg API-information i en lösenordshanterare som t.ex keyring
url = key_list(service = "nobil")$username
api_key = key_get("nobil", key_list(service = "nobil")$username)

laddst_sv <- GET(paste0(url, api_key, div))  #paste0 sammanfogar variablerna url, api_key och div utan mellanrum
laddst_sv_resp <- fromJSON(content(laddst_sv, as = "text"), flatten = FALSE)
laddst_sv_df <- laddst_sv_resp$chargerstations$csmd        #samtliga publika laddstationer i Sverige

#===Väljer ut laddstationer i Dalarnas län och väljer/byter namn på variabler
##ändra county_id till din region

laddst_dalarna <- laddst_sv_df %>%  
  filter(County_ID == 20) %>% 
  select(
    id, 
    namn = name,
    gata = Street,
    gatnr = House_number,
    postnr = Zipcode,
    ort = City,
    kom_kod = Municipality_ID,
    kommun = Municipality,
    # lan_kod = County_ID,
    # lan = County,
    lages_beskrivning = Description_of_location,
    agare = Owned_by,
    operator = Operator,
    anslutningspunkter = Available_charging_points,
    kommentar = User_comment,
    kontakt = Contact_info,
    skapad = Created,
    uppdaterad = Updated,
    station_status = Station_status,
    Position
  )
#skapar punktlagret med laddstationer
#====================punktlagret med laddstationer===============================

laddstationer_punkt <- laddst_dalarna$Position <- gsub("[()]", "", as.character(laddst_dalarna$Position)) #ta bort parenteser

laddstationer_punkt <- laddst_dalarna %>%  separate_wider_delim(Position, ",", names = c("lat", "lon")) #WGS84 Decimal (lat, lon) 

laddstationer_punkt <- st_as_sf(laddstationer_punkt, coords = c("lon", "lat"), 
                                crs = 4326, agr = "constant")

laddstationer <- laddstationer_punkt %>% 
  st_transform(crs = 3006)

# =====================================================================
#### NVDB från lokal drive G: (hämta från geodatabas i framtid)

nvdb <- "G:/Samhällsanalys/GIS/Grundkartor/nvdb/"
fil_nvdb <- "fixat_nvdb_riket.gpkg"  # oklart vad som är fixat, men filen väger mindre än nvdb_riket

sokvag_nvdb_sv <- paste0(nvdb, fil_nvdb)

nvdb_sv <- st_read(sokvag_nvdb_sv)  

nvdb_dalarna <- nvdb_sv %>% 
  filter(Vagnummer_Lanstillhorighet == 20) %>%         #ändra länstillhörighet till ditt regionid
  select("id" = "id",                                  #och rename
         # "antal korfalt" = "Antal_korfalt2_Korfaltsantal", 
         # "barighet" = "Barighet_Barighetsklass",
         # "vagklass" = "FunkVagklass_Klass", 
         # "gatunamn" = "Gatunamn_Namn",
         "hastighet_f" = "Hastighetsgrans_HogstaTillatnaHastighet_F",
         # "hastighet_b" = "Hastighetsgrans_HogstaTillatnaHastighet_B",
         # "Rekomenderad vag for farligt gods" = "RekomVagFarligtGods_Rekommendation",
         "ars_dygns_trafik" = "Trafik_ADT_fordon",
         # "vag bredd" = "Vagbredd_Bredd", 
         "vaghallare" = "Vaghallare_Vaghallartyp", 
         # "vagnr_europavag" = "Vagnummer_Europavag",
         "vagnummer" = "Vagnummer_Huvudnummer_Vard")


nvdb_dalarna <- nvdb_dalarna %>%
  # filter(vagnummer %in% c("16", "45", "50", "66", "68"))
  filter(vagnummer < 100)


# ersätt orter med rutor

# st_layers("G:/Samhällsanalys/GIS/projekt/rumslig_strategi/data/dag_natt_bef_ruta1km.gpkg")
enkmruta <- st_read("G:/Samhällsanalys/GIS/projekt/rumslig_strategi/data/dag_natt_bef_ruta1km.gpkg", crs = 3006)

# Antag att 'enkmruta' är ditt inlästa 1 km rutor lager

# Skapa ett 10 km rutnät baserat på den ursprungliga utbredningen av dina 1 km rutor
bbox <- st_bbox(enkmruta)
grid_10km <- st_make_grid(st_as_sfc(bbox), cellsize = c(10000, 10000), crs = st_crs(enkmruta))

# Gör om rutnätet till en sf-objekt
grid_10km_sf <- st_sf(geometry = grid_10km)

enkmruta_centroid <- enkmruta %>% 
  st_centroid()

# Spatial join mellan dina 1 km rutor och det nya 10 km rutnätet
joined_data_cent <- st_join(grid_10km_sf, enkmruta_centroid)

# Aggregera data för varje 10 km ruta
# Här antar vi att du vill summera 'dagbef' och 'nattbef' för varje 10 km ruta
aggregated_data <- joined_data_cent %>%
  group_by(geometry) %>%
  summarise(
    dagbef_sum = sum(dagbef, na.rm = TRUE),
    nattbef_sum = sum(nattbef, na.rm = TRUE)
  )

# lägg till ortnamn_ln20.gpkg
st_layers("G:/Samhällsanalys/GIS/Grundkartor/ort_namn/ortnamn_ln20.gpkg")
ortnamn <- st_read("G:/Samhällsanalys/GIS/Grundkartor/ort_namn/ortnamn_ln20.gpkg", crs = 3006)


rutor_med_ortnamn <- st_join(aggregated_data, ortnamn, join = st_nearest_feature) %>%
  group_by(geometry) %>%
  mutate(
    sockennamn = first(sockenstadnamn, na.rm = TRUE),
    ortnamn = first(ortnamn, na.rm = TRUE)
  ) %>%
  ungroup()  # Ta bort grupperingen om den inte längre behövs

# Antag att din sf-dataframe heter 'rutor_med_namn' och du har kolumnerna 'ortnamn' och 'sockenstadnamn'

# Slå ihop 'ortnamn' och 'sockenstadnamn' till en ny kolumn 'kombinerat_namn'
rutor_med_namn <- rutor_med_ortnamn %>%
  unite("socken_ortnamn", c("sockennamn", "ortnamn"), sep = "_", remove = FALSE) %>% 
  select(socken_ortnamn, dagbef_sum, nattbef_sum, lopnummer, geometry)

# # Kontrollera om det finns duplicerade värden i den nya kolumnen 'kombinerat_namn'
# duplicerade_namn <- rutor_med_namn$kombinerat_namn[duplicated(rutor_med_namn$kombinerat_namn)]
# 
# # Om 'duplicerade_namn' är tom, betyder det att alla namn är unika
# # Du kan också använda 'unique()' för att se de unika kombinationerna
# unika_namn <- unique(rutor_med_namn$kombinerat_namn)
# 
# # Skriv ut resultatet för att se om det finns duplicerade eller för att visa de unika namnen
# print(duplicerade_namn)
# print(unika_namn)

mapview(rutor_med_namn, label = "kombinerat_namn")

# ta bort rutor med natbef = 0 och dagbef = 0
rutor_med_namn <- rutor_med_namn %>%
  filter(dagbef_sum > 0 | nattbef_sum > 0)


# Nu har du ett nytt lager med 10 km rutor och aggregerad data
# mapview(grid_10km_sf)+
  mapview(enkmruta, zcol = "dagbef")+
  # mapview(joined_data)+
  mapview(aggregated_data, zcol = "dagbef_sum")

platser <- rutor_med_namn

platser <- platser %>% 
  mutate(unique_id = row_number())

# Skapa spatial join
vagar_per_ruta <- st_join(nvdb_dalarna, platser, join = st_within)

# Antag att vi har gjort en spatial join mellan 'nvdb_dalarna' och 'funktionella_orter'
# och resultatet är lagrat i 'vägar_per_ort'

# Beräkna högsta trafikvärden per väg och ort
hogsta_adt_plats <- vagar_per_ruta %>%
  group_by(vagnummer, unique_id) %>%  # Byt ut 'vagnummer' mot 'vagnr_europavag' om nödvändigt
  summarise(hogsta_trafik = max(ars_dygns_trafik, na.rm = TRUE), .groups = 'drop')

mapview(hogsta_adt_plats, label = "vagnummer", zcol = "unique_id", lwd = 5)+
  mapview(platser)

# Summera de högsta trafikvärdena för varje ort
sammanfattad_trafik_plats <- hogsta_adt_plats %>%
  group_by(unique_id) %>%
  summarise(total_adt = sum(hogsta_trafik), 
            max_adt = max(hogsta_trafik))

# Temporärt omvandla 'platser' till en vanlig dataframe genom att ta bort geometrikolonnen
platser_df <- st_set_geometry(platser, NULL)

# Utför join med 'sammanfattad_trafik_plats'
adt_plats <- left_join(platser_df, sammanfattad_trafik_plats, by = "unique_id")

# Omvandla tillbaka till sf-objekt, om nödvändigt
# Antag att 'platser' ursprungligen hade en geometrikolumn som du vill återställa
adt_plats_sf <- st_as_sf(adt_plats, geometry = st_geometry(platser), crs = st_crs(platser))

anslutningspunkter_plats <- st_join(platser, laddstationer) %>% 
  group_by(unique_id) %>%
  summarise(
    sum_anslutningspunkter = sum(anslutningspunkter, na.rm = TRUE),
    sum_stationer = n() 
  )
    
anslutningspunkter_plats <- anslutningspunkter_plats %>% 
  filter(sum_anslutningspunkter > 0)

# Ta bort geometrikolonnen temporärt för join-operationen
adt_plats_df <- st_set_geometry(adt_plats_sf, NULL)

# Utför join-operationen
indikator_per_ruta <- adt_plats_df %>%
  left_join(anslutningspunkter_plats, by = "unique_id") %>%
  mutate(indikator_max = round(max_adt / sum_anslutningspunkter, 0),
         indikator_tot = round(total_adt / sum_anslutningspunkter, 0))

# Omvandla tillbaka till sf-objekt om nödvändigt
indikator_per_ruta_sf <- st_as_sf(indikator_per_ruta, geometry = st_geometry(adt_plats_sf), crs = st_crs(adt_plats_sf))
# 
# greys <- colorRampPalette(c("grey50", "grey87"))

greens <- colorRampPalette(c("darkgreen", "green"))

#oranges <- colorRampPalette(c("orange4", "orange3", "orange2", "orange1", "orange"))

#Skapar kartan


# Om du vill ta bort rader där ALLA dessa kolumner är NA, använd 'filter()' som följande:
indikator_per_ruta_sf_rensat <- indikator_per_ruta_sf %>%
  filter(
    !(is.na(total_adt) & 
        is.na(max_adt) & 
        is.na(sum_anslutningspunkter) & 
        is.na(sum_stationer))
  )


mapview(nvdb_dalarna, zcol = "ars_dygns_trafik",label = "vagnummer", lwd = 2)+
  mapview(indikator_per_ruta_sf_rensat, zcol = "indikator_tot")+
  mapview(laddstationer, zcol = "anslutningspunkter", legend = FALSE, col.regions = "red", alpha.regions = 0.3, cex = "anslutningspunkter", homebutton = FALSE, layer.name = "Publika laddstationer")
        # popup = paste("Denna station har", laddstationer_punkt$anslutningspunkter, "anslutningspunkter"))
  # mapview(laddstationer_kom, zcol = "kommun", alpha.regions = 0, legend = FALSE, hide = TRUE, layer.name = "Kommungranser", homebutton = FALSE)

# Assuming indikator_per_ort_sf is your dataframe with the relevant columns.

# Filter out places without a valid indicator and order the plot
indikator_per_ruta_sf_rensat <- indikator_per_ruta_sf_rensat %>%
  filter(!is.na(indikator_max) & indikator_max > 0) %>%
  arrange(indikator_max)

# Create the plot
ggplot(indikator_per_ruta_sf_rensat, aes(x = reorder(socken_ortnamn, indikator_max), y = indikator_max)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Ruta", y = "Indikator Max", title = "Max Indikator per ruta") +
  coord_flip()  # To make the names appear on the y-axis and the indicators on the x-axis





