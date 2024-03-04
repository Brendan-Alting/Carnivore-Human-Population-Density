#2.Assign covariates to the individual 'traps' locations. 
#Using the KDEs to assign covariates to each camera station, for estimating sigma and g0. 

library(sf)
library(secr)
library(dplyr)
library(SpatialKDE)
library(tmap)
library(raster)

##read in traps file which contains Session ID, utms etc. 
trapsutmsixsessions <- read.csv(file = "Raw Data/TrapsUTMSpecificSixSessions.csv", header = T)

#assign to spatialpoints
trapcoords <- SpatialPoints(trapsutmsixsessions[, c("x", "y")])
###

#extract value for each trap coordinate from the mask create in '1.Habitat Mask' with raster values. 

kdeall_values <- extract(kde_all, trapcoords)

#assign these values back to the traps dataframe so that they can be used in SECR models later. 
trapsutmsixsessions$KdeAll <- kdeall_values$kde_value



#DONE
