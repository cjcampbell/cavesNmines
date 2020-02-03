
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
NoAm_sf_aea <- st_transform(NoAm_sf, crs = my_proj4string) %>% 
  sf::st_simplify(dTolerance = simplify_by)

plot_map <- geom_sf(
  data = NoAm_sf_aea, 
  size = 0.1, colour = "grey30", fill = NA
)

# Load karst maps ---------------------------------------------------------

mylayers <- wd$data %>% 
  list.files(recursive = TRUE, pattern = ".shp$", full.names = TRUE) %>% 
  grep("Continguous48", ., value = TRUE) %>% 
  lapply(sf::st_read) %>% 
  lapply(sf::st_simplify, preserveTopology = TRUE, dTolerance = 1e20) 

layer1_aea <- mylayers[[1]] %>% 
  st_transform(crs = my_proj4string) %>% 
  sf::st_simplify(dTolerance = simplify_by)

plot_karst <- ggplot() + 
  geom_sf(data = layer1_aea, fill = "grey90", color = "grey90" )


# Load points -------------------------------------------------------------

records_coded_tidy <- readRDS( file.path(wd$bin, "records_coded_tidy.rds") ) %>% 
  dplyr::mutate(Species_3 = if_else(Species == "LABL", "LABO", Species))

set.seed(420)
records_aea <- records_coded_tidy %>% 
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
    ) %>%
  st_transform(crs = my_proj4string) %>% 
  st_jitter(factor = 0.05)
  
plot_points <- geom_sf(
  data = records_aea,
  aes(fill = Species_3, shape = def_alive),
  size = 3, color = "black", alpha = 0.85
)

pt_bbox <- attributes(records_aea$geometry)$bbox

# plot together ----------------------------------------------------------

plot_karst + 
  plot_map +
  plot_points +
  coord_sf(
    xlim = c(pt_bbox$xmin - 2e6, pt_bbox$xmax + 8e5),
    ylim = c(pt_bbox$ymin - 5e5, pt_bbox$ymax + 5e5),
    expand = FALSE
  ) +
  scale_shape_manual(values = c(21,23), labels = c("No", "Yes")) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_line(color = "grey95"),
    legend.position = "none",
    axis.text = element_blank()
  )

# save maps ---------------------------------------------------------------


