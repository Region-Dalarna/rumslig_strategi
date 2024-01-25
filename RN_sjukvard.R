# Utbudspunkter
file_path <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/Offentlig_service/Utbudspunkter.csv"

library(sf)
library(dplyr)

# Läs in data
sjukvard <- read.csv(file_path, header = TRUE, sep = ";", fileEncoding = "ISO-8859-1")
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

mapview(sjukvard_sf)

# Load 'funktionella_orter' assuming it's an sf object and correctly loaded
funktionella_orter <- st_read("funktionella_orter.gpkg", crs = 3006)

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
    unique_sjukvard = n_distinct("")
  )

# Join the aggregates back to the funktionella_orter sf object
funktionella_orter_sjukvard <- st_join(funktionella_orter, sjukvard_aggregates, by = "unique_id")
mapview(funktionella_orter_sjukvard)
head(funktionella_orter_sjukvard)


regionalnod_sjukhus <- funktionella_orter_sjukvard %>%
  filter(has_SH == TRUE, has_VC == TRUE) %>% 
  st_centroid()

regionalnod_vardcentral <- funktionella_orter_sjukvard %>%
  filter(has_VC == TRUE) %>% 
  st_centroid()

mapview(regionalnod_sjukhus, cex = 10)+
  mapview(regionalnod_vardcentral, cex = 6)






