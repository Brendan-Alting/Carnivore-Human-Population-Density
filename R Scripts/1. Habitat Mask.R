
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
                  nx = 252, 
                  ny = 252) 

###Read in file with coordinates of potential anthropogenic resource supplements, and weights of each.
HPD <- read.csv(file = "Raw Data/CoordsCovs.csv", header = TRUE) %>%  
  st_as_sf(coords = c("x", "y"), crs = "epsg:7842") 


####Calculate the KDE weights for each 

cell_size <- 500
band_width <-5000 #This is estimated extent of resource supplement benefits

##Create grid to perform analysis on 
grid <- HPD %>%
  create_grid_rectangular(cell_size = cell_size, side_offset = (band_width)*3)

# fit kde's 
kde_all <- HPD %>%
  kde(band_width = band_width, kernel = "quartic", grid = grid, weights = HPD$WeightAlways, scaled = FALSE)


# log and scale kde
kde_all$kde_value <- log(kde_all$kde_value + 1)


# plot kde's to see how they look 
tm_shape(kde_all) +
  tm_polygons(col = "kde_value", palette = "viridis", title = "KDE Estimate") +
  tm_shape(HPD) +
  tm_bubbles(size = "WeightAlways", col = "white")



# ADD KDE TO MASK ---------------------------------------------------------
# convert to spatial points dataframe (this is what secr needs)
kde_all <- as_Spatial(kde_all)


# add covariates to secr mask - make a new one for each
mask <- addCovariates(mask, kde_all)


# check data- it's highly right skewed, even after the transformation
hist(covariates(mask)[,1])

# plot covariate

plot(mask, covariate = "kde_value", legend = TRUE, )

## --> SAVE MASK TO DERIVED DATA FOLDER 

