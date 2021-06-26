
if(!exists("records")) source(file.path(wd$R, "10_import_records.R"))

rerun_geocoding <- FALSE

records2 <- records %>% 
  dplyr::mutate( address = if_else(is.na(`Georeference Address`), `Location`, `Georeference Address`) )

if(rerun_geocoding != FALSE){
  # Prep for georeferencing
  require(ggmap)
  if(has_google_key() == FALSE){
    myAPIkey <- read.table("~/googleAPIkey.txt") %>% unlist %>% paste
    register_google(key = myAPIkey)
    if( has_google_key() ) message("Google API key registered.")
  }
  
  addresses <- records2 %>% 
    dplyr::select(address) %>% 
    distinct %>% 
    unlist
  
  coded <- lapply(addresses, function(i){
    x <- ggmap::geocode(
      i, output = "latlona", messaging = F, 
      override_limit = T, force = T, source = "google"
    )
    while(is.na(x$lon)){
      Sys.sleep(10)
      x <- geocode(
        i, output = "latlona", messaging = F, 
        override_limit = T, force = T, source = "google"
      )
    }
    Sys.sleep(1)
    tb <- cbind(ADDRESS = i, x)
    return(tb)
  } )
  coded_addresses <- coded %>% 
    plyr::ldply() %>% 
    dplyr::rename(input_address = ADDRESS, address_geocoded = address)
  
  save(coded_addresses, file = file.path(wd$bin, "coded_addresses.Rdata"))
} else {
  load(file.path(wd$bin, "coded_addresses.Rdata"))
}

records_coded <- left_join(records2, coded_addresses, by = c(address = "input_address") )

# Reclassify species now that we have lat/long.
records_coded <- records_coded %>% 
  dplyr::mutate(
  species_recorded = Species,
  species_presumed = if_else(Species == "LABO" & lon < -100, "LABL", Species) 
)

# Remove ambigious secondary records if a primary record exists prior to the publication date.
secondary_sources_state <- records_coded %>% 
  dplyr::filter(Source_primary_or_secondary == 2) %>% 
  filter(address == State) %>% 
  as.data.frame

auto_match_exists <- lapply(1:nrow(secondary_sources_state), function(i){
  records_coded %>% 
    dplyr::filter(Source_primary_or_secondary == 1) %>% 
    filter(
      `Latest possible year` <= secondary_sources[i, "Latest possible year"],
      species_presumed == secondary_sources[i, "species_presumed"],
      State == secondary_sources[i, "State"]
      ) %>% 
    nrow() %>% 
    return
})
if(sum(auto_match_exists == 0) != 0) error("auto match found some secondary state-level records that need to be looked at!")

# Manually look through other records. Reattach those that are legitimate.
secondary_sources_without_overlap <- records_coded %>% 
  dplyr::filter(Source_primary_or_secondary == 2) %>% 
  filter(address != State)  %>% 
  filter(
    Location == "Wyandotte Cave, Indiana" |
    Location == "Blanchard Springs Caverns, Ozark National Forest, Arkansas"
    )

records_coded_tidy <- records_coded %>% 
  dplyr::filter(Source_primary_or_secondary == 1) %>% 
  full_join(secondary_sources_without_overlap) %>% 
  dplyr::mutate(ID = make.names(row_number()))

saveRDS(records_coded_tidy, file = file.path(wd$bin, "records_coded_tidy.rds") )
