##3. Making capture history using SECR. 
#for this need the Dingo detection history, 

library(camtrapR)
library(dplyr)
library(secr)


#First read in the detection history, which has independent dingo events, event >30 minutes. 

doubleIDs <- read.csv(file = "Raw Data/DoubleIDs30min.csv")

#Read in camera trap functioning file
camopmatrixfoursessions <- read.csv(file = "Raw Data/CamOpMatrixFourSessions.csv", header = TRUE) 


#create camera operation file using camtrapR 

camopSDHsessionfour <- cameraOperation(CTtable = camopmatrixfoursessions,
                                       stationCol = "Trap",
                                       cameraCol = "Camera",
                                       sessionCol = "sessionCol",
                                       setupCol = "Setup_date",
                                       retrievalCol = "Retrieval_date",
                                       hasProblems = TRUE,
                                       camerasIndependent = FALSE,
                                       byCamera = FALSE,
                                       allCamsOn = FALSE,
                                       writecsv = FALSE,
                                       occasionStartTime = 0,
                                       dateFormat = "dmy")


##now we have the files needed for creating the spatial detection history used in SECR models.


##Now creating the spatial detection history, the main thing used when running the models. It uses a)the detections (30 min events), b) camera operability matrix, and c) trapsfile with covariates

dingoesfoursessions <- spatialDetectionHistory(recordTableIndividual = doubleIDs,
                                              camOp = camopSDHsessionfour,
                                              CTtable = trapsutmfoursessions,
                                              output = "binary",
                                              stationCol = "Trap",
                                              sessionCol = "Session",
                                              stationCovariateCols = c("TrailType", "Kde_value"),
                                              Xcol = "x", 
                                              Ycol = "y",
                                              species = "Dingo",
                                              individualCol = "Individual", 
                                              timeZone = "Australia/Sydney",
                                              recordDateTimeCol = "DateTime", 
                                              recordDateTimeFormat = "%Y-%m-%d %H:%M",
                                              occasionLength = 1,
                                              maxNumberDays = 180,
                                              day1 = "survey",
                                              includeEffort = TRUE)

##END