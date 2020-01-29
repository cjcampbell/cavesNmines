
source('~/cavesNmines/R/00_Setup.R')
records_coded_tidy <- readRDS( file.path(wd$bin, "records_coded_tidy.rds") )

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
  NoAm1 <- rgeos::gSimplify(NoAm, tol=0.01, topologyPreserve=TRUE)
  save(NoAm1, file = file.path( wd$bin, "NoAm1.Rdata") )
  
  # Get the main polygons, will determine by area.
  getSmallPolys <- function(poly, minarea=0.01) {
    # Get the areas
    areas <- lapply(poly@polygons, 
                    function(x) sapply(x@Polygons, function(y) y@area))
    
    # Quick summary of the areas
    print(quantile(unlist(areas)))
    
    # Which are the big polygons?
    bigpolys <- lapply(areas, function(x) which(x > minarea))
    length(unlist(bigpolys))
    
    # Get only the big polygons and extract them
    for(i in 1:length(bigpolys)){
      if(length(bigpolys[[i]]) >= 1 && bigpolys[[i]] >= 1){
        poly@polygons[[i]]@Polygons <- poly@polygons[[i]]@Polygons[bigpolys[[i]]]
        poly@polygons[[i]]@plotOrder <- 1:length(poly@polygons[[i]]@Polygons)
      }
    }
    return(poly)
  }
  NoAm2 <- getSmallPolys(NoAm1)
  save(NoAm2, file = file.path( wd$bin, "NoAm2.Rdata") )
  
  # Convert polygon to dataframe for use in ggplot2.
  NoAm_df <- broom::tidy(NoAm2)
  save(NoAm_df, file = file.path( wd$bin, "NoAm_df.Rdata") )
  
}

if(!exists("NoAm_df")) load(file.path( wd$bin, "NoAm_df.Rdata"))

require(ggrepel)


# Basic version -----------------------------------------------------------

plot_map <-   geom_polygon(
  data = NoAm_df, 
  aes(x = long, y = lat, group = group),
  size = 0.1, colour = "grey30", fill = "white"
)

ggplot(
  data = records_coded_tidy,
  aes(x = lon, y = lat)
) +
  plot_map +
  geom_point(
    aes(color = species_presumed, shape = def_alive),
    size = 3
  ) +
  scale_color_viridis_d() +
  coord_map("albers", lat0=30, lat1=40,
            xlim = c(-125,-70),
            ylim = c(25,55)
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_line(color = "grey95")#,
    #legend.position = "none"
  )


# Version 1 with individual point labels ----------------------------------

# Get sample size values.
records_coded_tidy_pooled <- records_coded_tidy %>% 
  dplyr::group_by(lat, lon, species_presumed, def_alive, Citation) %>% 
  dplyr::summarise( n = n() ) %>% 
  dplyr::mutate(
    lab = if_else( 
      n > 1,
      paste(Citation, ", n =", n ),
      Citation
      )
  )

ggplot(
  data = records_coded_tidy_pooled,
  aes(x = lon, y = lat)
  ) +
  plot_map +
  geom_point(
    aes(color = species_presumed, shape = def_alive),
    size = 3
  ) +
  geom_text_repel(
    aes(label = lab),
    point.padding = 1
  ) +
  scale_color_viridis_d() +
  #coord_quickmap(
  coord_map("albers", lat0=30, lat1=40,
    xlim = c(-125,-70),
    ylim = c(25,55)
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_line(color = "grey95")
  )

#ggsave(filename = file.path(wd$figs, "sample_map.png"))


# Version 2 with grouped point labels -------------------------------------

records_grouped <- records_coded_tidy %>% 
  dplyr::group_by(State, species_presumed, def_alive, Citation) %>% 
  dplyr::summarise( 
    n = n(),
    lat_min = min(lat),
    lat_mean = mean(lat),
    lon_mean = mean(lon)
    )

ggplot() +
  plot_map +
  coord_map("albers", lat0=30, lat1=40,
            xlim = c(-125,-70),
            ylim = c(25,55)
  ) +
  geom_text_repel(
    data = records_grouped,
    aes(x = lon_mean, y = lat_mean, label = Citation),
    point.padding = NA
  ) +
  geom_point( data = records_coded_tidy_pooled,
              aes(x = lon, y = lat, color = species_presumed, shape = def_alive),
              size = 3
  ) +
  scale_color_viridis_d() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_line(color = "grey95")
  )
