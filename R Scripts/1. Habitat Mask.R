
## 1) CREATE SECR HABITAT MASK
## 2) FIT SPATIAL KERNEL DENSITY ESTIMATES TO TOWNS / CAMPS 
## 3) ADD KDE TO HABITAT MASK 
## 4) 

# load packages
library(sf)
library(secr)
library(dplyr)
library(SpatialKDE)
library(tmap)
library(raster)

# 1) MAKE SECR HABITAT MASK ------------------------------------------------------
# load study area with wetlands removed 
study_area <- sf::read_sf("Raw Data/finalhabitatmapaug2023cut.shp") %>%
  filter(!(vegForm == "Freshwater Wetlands")) # remove non-habitat for dingoes

# read in cam locations to make a trapfile 
cams <- read.csv(file = "Raw Data/TrapsAlone.csv", header = TRUE)
traps <- read.traps(data = cams, detector = "proximity", trapID = "Trap")


### Make the masks using secr --Starting with small mask, increase later. 
mask <- make.mask(traps, 
                  type = 'trapbuffer', 
                  poly = study_area, 
                  buffer = 14000, 
                  keep.poly = F, 
                  nx = 160, 
                  ny = 160) 

###Read in file with coordinates of potential anthropogenic resource supplements, and weights of each.
ARS <- read.csv(file = "Raw Data/CoordsCovs.csv", header = TRUE) %>%  
  st_as_sf(coords = c("x", "y"), crs = "epsg:7842") 


####Calculate the KDE weights for each 

cell_size <- 500
band_width <-5000 #This is estimated extent of resource supplement benefits

##Create grid to perform analysis on 
grid <- ARS %>%
  create_grid_rectangular(cell_size = cell_size, side_offset = (band_width)*3)

# fit kde's 
kde_summer <- ARS %>%
  kde(band_width = band_width, kernel = "quartic", grid = grid, weights = ARS$WeightSummer, scaled = FALSE)

kde_shoulder <- ARS %>%
  kde(band_width = band_width, kernel = "quartic", grid = grid, weights = ARS$WeightShoulder, scaled = FALSE)

# log and scale kde
kde_summer$kde_value <- log(kde_summer$kde_value + 1)
kde_shoulder$kde_value <- log(kde_shoulder$kde_value + 1)

# plot kde's to see how they look 
tm_shape(kde_summer) +
  tm_polygons(col = "kde_value", palette = "viridis", title = "KDE Estimate") +
  tm_shape(ARS) +
  tm_bubbles(size = "WeightSummer", col = "white")

tm_shape(kde_shoulder) +
  tm_polygons(col = "kde_value", palette = "viridis", title = "KDE Estimate") +
  tm_shape(ARS) +
  tm_bubbles(size = "WeightShoulder", col = "white")


# ADD KDE TO MASK ---------------------------------------------------------
# convert to spatial points dataframe (this is what secr needs)
kde_summer <- as_Spatial(kde_summer)
kde_shoulder <- as_Spatial(kde_shoulder)

# add covariates to secr mask - make a new one for each
mask_summer <- addCovariates(mask, kde_summer)
mask_shoulder <- addCovariates(mask, kde_shoulder)


# check data- it's highly right skewed, even after the transformation
hist(covariates(mask_summer)[,1])
hist(covariates(mask_shoulder)[,1])

# plots
par(mfrow = c(1,2))
plot(mask_summer, covariate = "kde_value", legend = TRUE, )
plot(mask_shoulder, covariate = "kde_value", legend = TRUE)
allsessionmasks <- list(mask_summer, mask_shoulder, mask_summer, mask_shoulder)

## --> SAVE MASKS TO DERIVED DATA FOLDER 

