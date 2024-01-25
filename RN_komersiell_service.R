library(sf)
library(dplyr)
library(mapview)

# Base path for the files
kom_serv_filer <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/FrånPipos/"

# Function to read CSV, convert to sf, add abbreviation and service category
read_and_convert_to_sf <- function(file_path, x_col, y_col, crs, service_abbr, service_category) {
  df <- read.csv(file_path, header = TRUE, sep = ";", fileEncoding = "ISO-8859-1") %>%
    mutate(service_type_abbr = service_abbr, service_category = service_category) %>%
    st_as_sf(coords = c(x_col, y_col), crs = crs, agr = "constant")
  return(df)
}

# Reading files, converting to sf objects with categories and abbreviations
services_list <- list(
  read_and_convert_to_sf(paste0(kom_serv_filer, "apoteksvaror.csv"), "x", "y", 3006, "APK", "basic"),
  read_and_convert_to_sf(paste0(kom_serv_filer, "betalnings_förmedling.csv"), "x", "y", 3006, "BTF", "basic"),
  read_and_convert_to_sf(paste0(kom_serv_filer, "dagkasse_insättning.csv"), "x", "y", 3006, "DKI", "basic"),
  read_and_convert_to_sf(paste0(kom_serv_filer, "dagligvaror_EJ!_fullsortiment.csv"), "x", "y", 3006, "DVEJ", "basic"),
  read_and_convert_to_sf(paste0(kom_serv_filer, "dagligvaror_fullsortiment.csv"), "x", "y", 3006, "DVF", "commercial"),
  read_and_convert_to_sf(paste0(kom_serv_filer, "drivmedel_personbil.csv"), "x", "y", 3006, "DMP", "commercial"),
  read_and_convert_to_sf(paste0(kom_serv_filer, "posttjänster.csv"), "x", "y", 3006, "PST", "basic"),
  read_and_convert_to_sf(paste0(kom_serv_filer, "uttagsautomat.csv"), "x", "y", 3006, "UTA", "basic")
)

# Combine all services into one sf object
all_services_sf <- bind_rows(services_list)

# Load 'funktionella_orter' assuming it's an sf object and correctly loaded
funktionella_orter <- st_read("funktionella_orter.gpkg", crs = 3006)

# Perform spatial join with funktionella_orter
all_services_in_orter <- st_join(all_services_sf, funktionella_orter, join = st_within)

# Aggregate service information for each funktionell_ort
service_aggregates <- all_services_in_orter %>%
  group_by(unique_id) %>%
  summarise(
    sum_serv = n(),
    sum_grund_serv = sum(service_category == "basic"),
    sum_kom_serv = sum(service_category == "commercial"),
    unique_all_serv = n_distinct(service_category),
    unique_types = n_distinct(service_type_abbr)
  )

# Join the aggregates back to the funktionella_orter sf object
funktionella_orter_enriched <- st_join(funktionella_orter, service_aggregates, by = "unique_id")

# Identify and filter 'funktionella_orter' with at least one of each service type
# Definition: Kommersiell service samt en annan grundläggande service på platsen.   
# Med kommersiell service menas dagligvaruhandel och/eller drivmedel.  
# Med annan grundläggande service menas post-och pakettjänster, kontantuttag, betaltjänster eller  
# dagskassehantering, samt ombudstjänster för apoteksvaror.
eligible_orter_ids <- all_services_in_orter %>%
  group_by(unique_id) %>%
  summarise(both_types = all(c("basic", "commercial") %in% unique(service_category))) %>%
  filter(both_types) %>%
  pull(unique_id)

eligible_orter <- funktionella_orter[funktionella_orter$unique_id %in% eligible_orter_ids, ]

# Calculate centroids for regions with both service types
regionalnod_kommersiell_service <- st_centroid(eligible_orter)

# Visualize
mapview(funktionella_orter_enriched) +
  mapview(regionalnod_kommersiell_service, col.region = "black", cex = 17)+
  mapview(apotek_sf, col.region = "blue") +
  mapview(betalnings_formedling_sf, col.region = "red") +
  mapview(dagkasse_insattning_sf, col.region = "green") +
  mapview(dagligvaror_EJ_fullsort_sf, col.region = "yellow") +
  mapview(dagligvaror_fullsort_sf, col.region = "purple") +
  mapview(drivmedel_personbil_sf, col.region = "orange") +
  mapview(posttjanster_sf, col.region = "brown") +
  mapview(uttagsautomat_sf, col.region = "pink")
