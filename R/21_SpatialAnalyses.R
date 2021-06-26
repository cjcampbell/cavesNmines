source('~/cavesNmines/R/00_Setup.R')
library(raster)
library(sf)
library(fasterize)
records_coded_tidy <- readRDS(file.path(wd$bin, "records_coded_tidy.rds"))

myExtent <- c(xmin = -125, ymin = 18, xmax = -50, ymax = 60)


# Download env data -----------------------------------------------------------

wd$envdat <- file.path(wd$bin, "envDat")
if(!dir.exists(wd$envdat)) dir.create(wd$envdat)

# GADM Data (for extent analyses...)
USA <- raster::getData('GADM', country = "USA", level = 1, path = wd$envdat)
CAN <- raster::getData('GADM', country = "CAN", level = 1, path = wd$envdat)

# Worldclim data.
wc <- raster::getData('worldclim', res = 2.5, var = 'bio', path = wd$envdat)

# Elevation data.
ele_USA <- raster::getData("alt", country = "USA", mask = TRUE, path = wd$envdat)
ele_CAN <- raster::getData("alt", country = "CAN", mask = TRUE, path = wd$envdat)


# Download day weather data -----------------------------------------------

# First, identify obs with complete dates since 1980. (Include live and non-live ones for now.)

daymetTargetRecords <- records_coded_tidy %>% 
  dplyr::mutate(Year = as.numeric(`Timing of record`)) %>% 
  dplyr::filter(!is.na(Day), !is.na(Month), !is.na(Year), !is.na(lat), !is.na(lon)) %>% 
  filter(Year >= 1980)
nrow(daymetTargetRecords)

i <- 1
daymetTargetRecords[i,]

savePath <- file.path(wd$bin, "daymet", daymetTargetRecords[i,"ID"])

eventDate <- paste(
  sep = "-",
  daymetTargetRecords[i,"Year"],
  daymetTargetRecords[i,"Month"],
  daymetTargetRecords[i,"Day"]
)
lat2enter <- round(daymetTargetRecords[i,"lat"], digits = 4)

constructURL <- paste(
  sep = "&",
  "https://daymet.ornl.gov/single-pixel/api/data",
  paste0( "lat=",   lat2enter),
  paste0( "lon=",   daymetTargetRecords[i,"lon"]),
  paste0( "vars=",  "dayl,prcp,srad,swe,tmax,tmin,vp"),
  paste0( "start=", eventDate),
  paste0( "end=",   eventDate)
  )
download.file(url = constructURL, destfile = savePath, extra = "--content-disposition")








# Combine env dat ----------------------------------------------------------

# GADM data.
GADM <- rbind(USA, CAN) %>% 
  sf::st_as_sf(.) %>%
  st_crop(myExtent) %>% 
  st_simplify(preserveTopology = FALSE, dTolerance = .1)
saveRDS(GADM, file = file.path(wd$bin, "GADM.rds"))


# # Elevation data.
# 
# 
# 
#  I HAVE NOT YET FIGURED OUT THE ELEVATION DATA :(
# 
# aldData_path <- file.path(wd$envdat, "altData_raster.grd")
# if(!file.exists(aldData_path)){
#   
#   altData_list <- list.files(wd$envdat, full.names = T, pattern = "alt.grd") %>% 
#     lapply(raster::raster)
#   
#   altData_raster <- a[[1]]
#   for(i in 2:length(a)) {
#     altData_raster <- mosaic(altData_raster, a[[i]], fun = max)
#   }
#   
#   writeRaster(altData_raster, file = altData_path)
#   
# }
# 
# # Karst data
# rasterizeMe <- function(a1){
#   if(!exists("GADM") ) { GADM <- readRDS(file.path(wd$bin, "GADM.rds")) }
#   
#   a2 <- a1[!st_is_empty(a1),,drop=FALSE]
#   a3 <- a2 %>% st_cast()
#   
#   if( "MULTIPOLYGON" %in% unique(st_geometry_type(a2)) ) {
#     k1 <- fasterize::raster(GADM)
#     k1_rast <- fasterize(a3, k1)
#     return(k1_rast)
#   } else {
#     return(NA)
#   }
# 
# }
# 
# a <- wd$data %>% 
#   list.files(recursive = TRUE, pattern = ".shp$", full.names = TRUE) %>% 
#   grep("Continguous48", ., value = TRUE) %>% 
#   lapply(., function(a){
#     sf::st_read(a) %>%
#       sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000 )
#   })
# 
# b <- lapply(a, rasterizeMe)
#   
# na.omit(b)
