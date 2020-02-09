
# Setup -------------------------------------------------------------------

source('~/cavesNmines/R/00_Setup.R')
require(sf)
require(ggplot2)
require(dplyr)

# Set projection
my_proj4string <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Simplify_by
simplify_by <- 1e4

# load boundary maps ------------------------------------------------------

# Generate / load map data
makemap <- FALSE
if(makemap == TRUE){
  
  # Get GADM data to country level.
  USA <- raster::getData('GADM', country='USA', level = 1, path = wd$bin)
  MEX <- raster::getData('GADM', country='MEX', level = 1, path = wd$bin)
  CAN <- raster::getData('GADM', country='CAN', level = 1, path = wd$bin)
  
  # Remove areas outside of desired extent
  USA <- USA[USA@data$NAME_1 != "Hawaii",] # Remove Hawaii
  
  # Combine into one polygon; (optionally) simplify.
  NoAm <- rbind(MEX, USA, CAN)
  save(NoAm, file = file.path( wd$bin, "NoAm.Rdata") )
  
  NoAm_sf <- st_as_sf(NoAm)
  
  save(NoAm_sf, file = file.path(wd$bin, "NoAm_sf.Rdata"))
}

if(!exists("NoAm_sf")) load(file.path( wd$bin, "NoAm_sf.Rdata"))

st_crs(NoAm_sf)
NoAm_sf_aea <- st_transform(NoAm_sf, crs = my_proj4string) #%>% 
  #sf::st_simplify(dTolerance = simplify_by)

plot_map_whiteFill <- geom_sf(
  data = NoAm_sf_aea, 
  size = 0.5, colour = "black", fill = "white"
)
plot_map_noFill <- geom_sf(
  data = NoAm_sf_aea, 
  size = 0.5, colour = "black", fill = NA
)


# Load karst maps ---------------------------------------------------------
loadlayers <- TRUE
if(loadlayers == TRUE){
  mylayers <- wd$data %>% 
    list.files(recursive = TRUE, pattern = ".shp$", full.names = TRUE) %>% 
    grep("Continguous48", ., value = TRUE) %>% 
    lapply(sf::st_read) %>% 
    lapply(sf::st_simplify, preserveTopology = TRUE, dTolerance = 1e20) 
  
  layer1_aea <- mylayers[[1]] %>% 
    st_transform(crs = my_proj4string) #%>% 
    #sf::st_simplify(dTolerance = simplify_by)
  
  save(layer1_aea, file = file.path(wd$bin, "layer1_aea.Rdata"))
} else load(file.path(wd$bin, "layer1_aea.Rdata"))

plot_karst <- geom_sf(data = layer1_aea, fill = "grey90", color = "grey90" )

# Load points -------------------------------------------------------------
ord_species <- c("LACI", "LABO", "LANO")

records_coded_tidy <- readRDS( file.path(wd$bin, "records_coded_tidy.rds") ) %>% 
  dplyr::mutate(Species_3 = if_else(Species == "LABL", "LABO", Species)) %>% 
  dplyr::mutate(Species_3 = factor(Species_3, levels = ord_species))

set.seed(2)
records_aea <- records_coded_tidy %>% 
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
    ) %>%
  st_transform(crs = my_proj4string) %>% 
  st_jitter(factor = 0.02)
  
plot_points <- geom_sf(
  data = records_aea,
  aes(fill = Species_3, shape = def_alive),
  size = 11, color = "black", alpha = 0.85, stroke = 1.6
)

pt_bbox <- attributes(records_aea$geometry)$bbox

# plot together ----------------------------------------------------------

xmin <- pt_bbox$xmin - 20e5
xmax <- pt_bbox$xmax + 10e5
ymin1 <- pt_bbox$ymin - 11e5
ymax1 <- pt_bbox$ymax + 12e5

# Make them ratio-correct with slide:
# ymid <- (ymax1+ymin1)/2
# ymax <- ymid + ((xmax-xmin) * 36 / 48 / 2)
# ymin <- ymid - ((xmax-xmin) * 36 / 48 / 2)
ymax <- ymax1
ymin <- ymin1 - 296713


bigmap <- ggplot() +
  plot_map_whiteFill+
  plot_karst + 
  plot_map_noFill +
  plot_points +
  coord_sf(
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    expand = FALSE
  ) +
  scale_shape_manual(values = c(21,23), labels = c("No", "Yes")) +
  scale_fill_manual(values = rev(viridis::plasma(3))) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey50"),
    legend.position = "none",
    axis.text = element_blank(),
    plot.margin = unit(c(0,0,0,0), "in")
  )

ggsave(bigmap, filename = file.path(wd$figs, "bigmap.tif"), device = "tiff", width = 48, height = 36, units = "in", dpi = 300)

bigmap_pointOnly <- ggplot() +
  plot_points +
  coord_sf(
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    expand = FALSE
  ) +
  scale_shape_manual(values = c(21,23), labels = c("No", "Yes")) +
  scale_fill_manual(values = rev(viridis::plasma(3))) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text = element_blank(),
    plot.margin = unit(c(0,0,0,0), "in"),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )
ggsave(bigmap_pointOnly, filename = file.path(wd$figs, "bigmap_pointOnly.tif"), device = "tiff", width = 48, height = 36, units = "in", dpi = 300, bg = "transparent")
