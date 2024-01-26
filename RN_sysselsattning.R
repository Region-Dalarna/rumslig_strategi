#=================sysselsättning=================

dagbef <- st_read("dag_natt_bef_ruta1km.gpkg") |> 
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

# mapview(regionalnod_sysselsattning, zcol = "sum_dagbef", cex = "sum_dagbef")+
#   mapview(tatort_layer, col.regions = "blue", alpha.regions = 0.3)
# 
# st_write(funktionella_orter_sysselsattning, "funktionella_orter.gpkg", layer = "funktionella_orter_sysselsattning", driver = "GPKG")
