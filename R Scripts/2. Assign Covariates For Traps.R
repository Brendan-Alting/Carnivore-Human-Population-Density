#2.Assign covariates to the individual 'traps' locations. 
#Using the KDEs to assign covariates to each camera station, for estimating sigma and g0. 

library(sf)
library(secr)
library(dplyr)
library(SpatialKDE)
library(tmap)
library(raster)

##read in traps file which contains Session ID, utms etc. 
trapsutmfoursessions <- read.csv(file = "Raw Data/TrapsUTMSpecificFourSessions.csv", header = T)

#assign to spatialpoints
trapcoords <- SpatialPoints(trapsutmfoursessions[, c("x", "y")])
###

#extract value for each trap coordinate from the mask create in '1.Habitat Mask' with raster values. 

kdesummer_values <- extract(kde_summer, trapcoords)
kdeshoulder_values <- extract(kde_shoulder, trapcoords)

#assign these values back to the traps dataframe so that they can be used in SECR models later. 
trapsutmfoursessions$KDEsummersupp <- kdesummer_values$kde_value
trapsutmfoursessions$KDEshouldersupp <- kdeshoulder_values$kde_value

#select values for specific sessions. This will just assign them to a new column, where they match their session value.  
trapsutmfoursessions$Kde_value <- ifelse(trapsutmfoursessions$Session %in% c(1, 3), trapsutmfoursessions$KDEsummersupp,
                                             ifelse(trapsutmfoursessions$Session %in% c(2, 4), trapsutmfoursessions$KDEshouldersupp, NA))

#DONE
