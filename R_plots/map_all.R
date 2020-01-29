
# Setup -------------------------------------------------------------------

source('~/cavesNmines/R/00_Setup.R')
require(sf)
require(ggplot2)

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

plot_map <- geom_sf(
  data = NoAm_sf, 
  size = 0.1, colour = "grey30", fill = NA
)

# Load karst maps ---------------------------------------------------------

mylayers <- wd$data %>% 
  list.files(recursive = TRUE, pattern = ".shp$", full.names = TRUE) %>% 
  grep("Continguous48", ., value = TRUE) %>% 
  lapply(sf::st_read) %>% 
  lapply(sf::st_simplify, preserveTopology = TRUE, dTolerance = 1e20) 

plot_karst <- ggplot() + 
  geom_sf(data = mylayers[[1]], fill = "grey90", color = "grey90" ) +
  geom_sf(data = mylayers[[7]], fill = "grey90", color = "grey90" )

# Load points -------------------------------------------------------------

records_coded_tidy <- readRDS( file.path(wd$bin, "records_coded_tidy.rds") )

plot_points <-   geom_point(
  data = records_coded_tidy,
  aes(x = lon, y = lat, color = species_presumed, shape = def_alive),
  size = 3
)

# plot together ----------------------------------------------------------

plot_karst + 
  plot_map +
  plot_points +
  scale_color_viridis_d() +
  coord_sf(
    xlim = c(-125,-70),
    ylim = c(25,55)
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_line(color = "grey95")#,
    #legend.position = "none"
  )



# save maps ---------------------------------------------------------------


