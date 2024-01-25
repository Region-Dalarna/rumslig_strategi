# hämta geojson fråmn G:/Samhällsanalys/GIS/projekt/gtfs/trafiklab_dt
# stops.geojson
# routes.geojson

# stops_sf <- st_read("G:/Samhällsanalys/GIS/projekt/gtfs/trafiklab_dt/stops.geojson")
# routes_sf <- st_read("G:/Samhällsanalys/GIS/projekt/gtfs/trafiklab_dt/routes.geojson")
# head(stops_sf)
# head(routes_sf)
# plot(st_geometry(stops_sf))
# plot(st_geometry(routes_sf), add = TRUE, col = 'red')
# mapview(stops_sf)+
#   mapview(routes_sf)

# G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/kollektivtrafik
# tatorter_noder_alla_turer
# reg_nod_koll_fil <- "G:/Samhällsanalys/GIS/projekt/Regionala Noder/indata/kollektivtrafik/"
# reg_nod_kolltrafik <- read.csv(reg_nod_koll_fil, )
# 
# read_and_convert_to_sf <- function(file_path, x_col, y_col, crs) {
#   df <- read.csv(file_path, header = TRUE, sep = ";", fileEncoding = "ISO-8859-1") %>% 
#     st_as_sf(coords = c(x_col, y_col), crs = crs, agr = "constant")
#   return(df)
# }
# 
# # Reading files, converting to sf objects with categories and abbreviations
# 
# regionalnod_kollektivtrafik <- read_and_convert_to_sf(paste0(reg_nod_koll_fil, "tatorter_noder_alla_turer.csv"), "x", "y", 3006, "APK", "basic"),






