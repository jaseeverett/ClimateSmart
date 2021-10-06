# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This code intersects the cost layer and the study area (in PUs).
# creates a .rds file and .shp files of the PUs x cost layer.
# Inputs include the following:
# 1. input: raster file of the cost layer
# 2. pu_shp: .shp or .rds file of the PUs
# 3. outdir: path of the output
# 4. layer: "all" or "pelagics"
# 5. stack_num: if layer == "all": NA, if layer == "pelagics", input the stack number (if multiple use c())
  
fCostPU <- function(input, pu_shp, outdir, layer, stack_num, window_size, ...) {

  ########################################
  ####### Defining packages needed #######
  ########################################
  list.of.packages <- c("tidyverse", "raster", "sf", "magrittr", "rnaturalearth",
                        "rnaturalearthdata", "fasterize", "proj4", "exactextractr")
  # If is not installed, install the pacakge
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # Load packages
  lapply(list.of.packages, require, character.only = TRUE)
  
  ########################################
  ####### Defining generalities #######
  ########################################
  rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  ################################################
  ####### Calling the PU shape file / .rds #######
  ################################################
  if(stringr::str_detect(string = pu_shp, pattern = ".rds") == TRUE) {
    shp_PU_sf <- readRDS(pu_shp)
  } else if (stringr::str_detect(string = pu_shp, pattern = ".shp") == TRUE) {
    shp_PU_sf <- st_read(pu_shp)
  }
  # transforming into robinson's projection
  shp_PU_sf <- shp_PU_sf %>% 
    st_transform(crs = rob_pacific)
      
  shp_PU_sf1 <- shp_PU_sf %>%
    dplyr::mutate (cellsID = 1:nrow(shp_PU_sf), 
                   area_km2 = as.numeric(st_area(shp_PU_sf)/1e+06)) %>% 
    dplyr::select(cellsID, geometry)
  pu_min_area <- min(shp_PU_sf1$area_km2)
  
  ################################################
  ####### Calling the Raster of Cost Layer #######
  ################################################    
  if(layer == "pelagics") {
    temp_cost <- stack(input)
    temp_cost1 <- raster::subset(temp_cost, stack_num)
    epi_cost <- calc(temp_cost1, fun = sum)
    #epi_cost <- unstack(temp_cost)[[stack_num]]
  }else {
    epi_cost <- readAll(raster(input))
  }
  
  ###############
  ## Smoothing ##
  ###############

  WindowSize = window_size
  epi_cost <- focal(epi_cost, w = matrix(1, WindowSize, WindowSize), na.rm = TRUE, pad = TRUE, fun = mean)
  
  crs(epi_cost) <- CRS(longlat)
    
  #####################################
  ####### Intersecting with PUs #######
  ##################################### 
  # Creating layer of weights
  weight_rs <- raster::area(epi_cost)
      
  # Projecting the costs and weights into Robinson's (the same projection as the PUs)
  cost_filef <- projectRaster(epi_cost, crs = CRS(rob_pacific), method = "ngb", over = FALSE, res = 669.9)
  weight_rsf <- projectRaster(weight_rs, crs = CRS(rob_pacific), method = "ngb", over = FALSE, res = 669.9)
      
  names(cost_filef) <- "layer"

  # Getting cost value by planning unit
  cost_bypu <- exact_extract(cost_filef, shp_PU_sf1, "weighted_mean", weights = weight_rsf)
  
  pu_file <- shp_PU_sf1 %>% 
    dplyr::mutate(cost = cost_bypu)
  
  ####################################
  ## 'Fixing' 180 deg longitude ##
  ####################################
  coord_df <- sf::st_centroid(pu_file) %>% 
    st_coordinates() %>% 
    as_tibble()
  pu_file$x <- coord_df$X
  pu_file$y <- coord_df$Y

  test <- cbind(c('180','0.5'))
  CnR <- proj4::project(test, proj = rob_pacific)
  
  temp <- pu_file %>% 
    dplyr::filter(between(x, -CnR[1,2], CnR[1,2]))
  temp1 <- pu_file %>% 
    dplyr::filter(!between(x, -CnR[1,2], CnR[1,2]))
  nr <- st_nearest_feature(temp, temp1)
  
  pu_file1 <- pu_file %>% 
    mutate(cost = replace(cost, between(x, -CnR[1,2], CnR[1,2]), temp1$cost[nr]))
  
  pu_file <- pu_file1
  
  #################
  ## Final edits ##
  #################
  
  small_value <- (min(pu_file$cost, na.rm = T))/2

  pu_filef <- pu_file %>% 
    dplyr::mutate(cost = ifelse(is.na(cost), small_value, cost)) %>% 
    dplyr::mutate(cost_log = log10(cost+1)) %>% 
    dplyr::mutate(cost_categ = ifelse(cost_log == 0, 1,
                                ifelse(cost_log > 0 & cost_log <= 1, 2,
                                  ifelse(cost_log > 1 & cost_log <= 2, 3,
                                    ifelse(cost_log > 2 & cost_log <= 3, 4,
                                      ifelse(cost_log > 3 & cost_log <= 4, 5, 6))))))
  #####################################
  ####### Saving files #######
  #####################################       
  saveRDS(pu_filef, paste0(outdir, "costlayer.rds"))
      
  return(pu_filef)
  }

