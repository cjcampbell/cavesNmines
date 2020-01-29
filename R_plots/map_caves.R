require(sf)
require(ggplot2)

mylayers <- wd$data %>% 
  list.files(recursive = TRUE, pattern = ".shp$", full.names = TRUE) %>% 
  grep("Continguous48", ., value = TRUE) %>% 
  lapply(sf::st_read) %>% 
  lapply(sf::st_simplify, preserveTopology = TRUE, dTolerance = 1e20) 

karst <- bind(mylayers[[1]], mylayers[[2]])

ggplot() +
  geom_sf(data = karst, fill = "grey90", color = "grey90" )
