# This code is written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This code creates the cost layer for the planning region

source("scripts/helper_fxns/fCostPU.R")

#### Creating cost layer including all functional groups ####
input = "inputs/rasterfiles/CostLayer/Cost_RasterStack_byFunctionalGroup.grd"
pu_shp = "outputs/01_PlanningRegion/WestPacific_025deg.rds"
outdir = "outputs/02_CostLayer/"
layer = "all"
stack = NA
window_size = 5

cost <- fCostPU(input, pu_shp, outdir, layer, stack, window_size)

#### Plotting cost ####
library(RColorBrewer)
library(patchwork)
library(sf)
# defining palette
myPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))
sc <- scale_colour_gradientn(name = "log10(cost)", colours = myPalette(100), limits=c(0, 4), aesthetics = c("color","fill"))

rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
world_sf <- land_robinson #land_robinson is from 01_PlanningRegion.R

# creating boundaries
source("scripts/study_area/fCreateRobinsonBoundary.R")
Bndry <- fCreateRobinsonBoundary(west = 125, east = 115                                                                                                                                                                                                , north = 30, south = 55)

# plot using ggplot
cost_plot <- ggplot()+
  geom_sf(data = cost, aes(color = cost_log, fill = cost_log)) +
  sc +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  coord_sf(xlim = c(st_bbox(Bndry)$xmin, st_bbox(Bndry)$xmax), 
           ylim = c(st_bbox(Bndry)$ymin, st_bbox(Bndry)$ymax),
           expand = TRUE) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 25),
        legend.key.width = unit(1,"cm"))
ggsave("outputs/02_CostLayer/cost_smoothened.png", width = 20, height = 10, dpi = 600)
