fCreateSinglePolygon <- function (df, res){
  
  library(raster)
  library(fasterize)
  
  # Creating a empty raster
  rs <- raster(ncol = 360*(1/res), nrow = 180*(1/res)) 
  rs[] <- 1:ncell(rs)
  crs(rs) <- CRS(longlat)
  
  # Fasterize the land object
  df_rs <- fasterize(df, rs)
  
  pol <- as(df_rs,  "SpatialPolygonsDataFrame")
  pol$layer <- seq(1, length(pol))
  
  # Now to a sf object and create ONE BIG polygon that we can use to populate with PUs
  pol_sf <- st_as_sf(pol) %>% 
    dplyr::select(layer) %>% 
    summarise(total_layer = sum(layer, do_union = TRUE))
}