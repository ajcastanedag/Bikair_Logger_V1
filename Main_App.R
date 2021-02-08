# import functions file 
ipak <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) 
    
    install.packages(new.pkg, dependencies = TRUE)
  
  sapply(pkg, require, character.only = TRUE)
  
}
########################################################################################
# Load libraries
ipak(c("sp","raster","rlist","sf","sp","list","leaflet","ggplot2","gganimate",
       "ggmap","pals","ggdark","reshape2","RColorBrewer","plyr", "hms","stringi",
       "shiny","shinyWidgets","shinythemes", "patchwork","vroom","shinyjs","leaflet.extras2",
       "colourpicker","gstat", "DT","shinybusy","patchwork","ggmap", "mapview", "htmlwidgets",
       "tidyr","leafem"))
########################################################################################
addResourcePath(prefix = 'pics', directoryPath = paste0(getwd(),"/www"))
########################################################################################
source("Ui.R")
source("Server.R")
########################################################################################
shinyApp(ui, server)
########################################################################################
#write.table(Data,"tester.txt",sep = ",",row.names = F,quote = F)
