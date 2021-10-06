# This code is written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This code creates the planning region with a 0.25 x 0.25 degree equal area resolution

#### Defining the main packages ####

library(raster)
library(sf)
library(tidyverse)
library(magrittr)
library(rnaturalearth)
library(proj4)
library(terra) # try and replace functions that are of raster to terra

#### Defining generalities and calling helper functions ####

rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# different functions for creating the study area
source("scripts/study_area/fConvert2PacificRobinson.R")
source("scripts/study_area/fCreateSinglePolygon.R")
source("scripts/study_area/fCreateMaskedPolygon.R")
source("scripts/study_area/fCreate_PlanningUnits.R")
source("scripts/study_area/fCreateRobinsonBoundary.R")
# Function to download shapefile ----------------------
get_shape <- function(shp_url, shp_name) { # Pass the shapefile zip URL to the function
  tmp <- tempfile() # Temporary file to catch the download
  download.file(shp_url, tmp) # Download the shapefile zip to the temporary folder
  tmp_dir <- unlist(strsplit(tmp, paste0("/", basename(tmp)))) # Folder in which temp files are placed (might be equivalent to tempdir())
  unzip(tmp, exdir = tmp_dir) # Unzip it in the temporary directory
  return(st_read(paste0(tmp_dir, "/", shp_name))) # Return the shapefile as an sf object (this assumes a single .shp in the zip)
}

#### Setting boundary for study area ####
# 125E, 115W, 30N, 55S
Bndry <- fCreateRobinsonBoundary(west = 125, east = 115, north = 30, south = 55)

#### EEZs ####
res = 0.5
mask = st_read("inputs/shapefiles/World_EEZ_v11_20191118/eez_v11.shp") %>% #from Marine Regions
  filter(SOVEREIGN1 != "Antarctica")
inverse = FALSE
eez_robinson <- fCreateMaskedPolygon(ocean_temp, res, mask, inverse) %>% 
  fConvert2PacificRobinson()

ggplot() +
  geom_sf(data = eez_robinson) +
  geom_sf(data = land_robinson, fill = "grey64") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), # Set limits based on Bndry bbox
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE)

#### Creating Pacific-centered and Robinson projected land masses ####
land_sf <- get_shape("https://naturalearth.s3.amazonaws.com/10m_physical/ne_10m_land.zip", "ne_10m_land.shp")
land_robinson <- fConvert2PacificRobinson(land_sf)
ggplot() +
  geom_sf(data = land_robinson, fill = "grey64")

#### Creating Pacific-centered Western Pacific Area ####
ocean_sf <- get_shape("https://naturalearth.s3.amazonaws.com/10m_physical/ne_10m_geography_marine_polys.zip", "ne_10m_geography_marine_polys.shp") 
ocean_sf %<>% 
  dplyr::select(label, ne_id, geometry)
unique(ocean_sf$label)

ocean_temp <- ocean_sf %>% 
  filter (label %in% c("NORTH PACIFIC OCEAN", "SOUTH PACIFIC OCEAN", "Philippine Sea", "Coral Sea", "Tasman Sea", "Bay of Plenty", "Cook Strait", 
                       "Bismarck Sea", "Solomon Sea", "Bass Strait"))

# Project it to Robinson's Projection
pacific_robinson <- fConvert2PacificRobinson(ocean_temp)
# getting coordinates
coord <- cbind(pacific_robinson, st_coordinates(st_centroid(pacific_robinson$geometry)))

# Plotting it
ggplot() +
  geom_sf(data = pacific_robinson, fill = "lightsteelblue2") +
  geom_sf(data = land_robinson, fill = "grey45") +
  geom_sf(data = eez_robinson, fill = "royalblue4", alpha = 0.5) +
  geom_text(data = coord, aes(x = X, y = Y, label=label),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), # Set limits based on Bndry bbox
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE)

#### Creating Planning Units ####
# 125E, 115W, 30N, 55S
Bndry <- fCreateRobinsonBoundary(west = 125, east = 115, north = 32, south = 55)
LandMass <- eez_robinson
CellArea <- 669.9 # kms2 for 0.5 degree resolution
Shape = "Hexagon" # Hexagon or Square

PUsWPac <- fCreate_PlanningUnits(Bndry, LandMass, CellArea, Shape)
# Saving PUsWPac
saveRDS(PUsWPac, file = "outputs/01_PlanningRegion/WestPacific_025deg.rds")

study_area <- ggplot() +
  geom_sf(data = land_robinson, color = "grey20", fill="grey20", size = 0.1, show.legend = FALSE) +
  geom_sf(data = PUsWPac, colour = "grey64", fill = "steelblue", size = 0.1, show.legend = FALSE) + 
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), # Set limits based on Bndry bbox
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))
ggsave("outputs/01_PlanningRegion/planning_region.png", width = 20, height = 10, dpi = 600)

