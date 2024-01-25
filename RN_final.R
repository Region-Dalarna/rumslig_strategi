










mapview(tatort_layer, col.regions = "blue", alpha.regions = 0.2)+
  mapview(funktionella_orter_enriched, col.regions = "lightblue", alpha.regions = 0.2)+
  mapview(regionalnod_sjukhus, col.regions = "red", cex = 15)+
  mapview(regionalnod_vardcentral, col.regions = "pink", cex = 12)+
  mapview(regionalnod_kommersiell_service, col.regions = "blue", cex = 9)+
  mapview(regionalnod_bredband, col.regions = "green", cex = 6)+
  mapview(regionalnod_sysselsattning, col.regions = "orange", cex = 3)



