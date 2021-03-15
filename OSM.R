library(ggplot2)
library(leaflet)
library(rgdal)

Data_T <- Data


write.csv(Data_T,"Teste.csv", row.names = FALSE)

Data_T$Dist <- 0

for(i in 1:length(Data_T$LAT)-1){
  Data_T$Dist[i] <- sqrt(((Data_T$LAT[i+1] - Data_T$LAT[i])^2) + ((Data_T$LON[i+1] - Data_T$LON[i])^2))

}

for(i in 1:length(Data_T$LAT)-1){
  Data_T$Vel[i] <- Data_T$Dist[i]/1
}


ggplot(Data_T) +
  geom_point(aes(x=ID, y=Vel))

########################################################################################
library(osrm)





# Repeat and merge

for(i in 1:(length(SpDf)/100)-1){
  print(i+1)
}

start <- 1
end <- start + 99


129 - 517


# Call osrm 
trips <- osrmTrip(loc = SpDf[c(129,517),], returnclass="sp")
# Extract info
trip <- trips[[1]]$trip
# Convert to points
trip_pt = as(trip, "SpatialPointsDataFrame")


leaflet(trip) %>% 
  addProviderTiles(providers$CartoDB.Positron, group = 'Cartographic',
                   options = providerTileOptions(opacity = 0.9)) %>%
  addPolylines(data=trip) %>%
  #addCircles(data = SpDf[241:340,], color="blue", radius = 1) %>%
  addCircles(data = trip_pt, color="green", radius = 1) 
  


