
# I manually coded data in Excel 2019-12-15. Easier!
codedDat <- read.csv(file = file.path(wd$data, "codedDat.csv"))

theseDat <- codedDat %>% 
  # Note that we are EXCLUDING fossil records here.
  filter(validated == 1) %>% 
  dplyr::select(
    species, sex, lifeStage,
    validationText,
    day, month, year,
    county, stateProvince, higherGeography,
    decimalLatitude, decimalLongitude,
    basisOfRecord, habitat,
    catalogNumber, recordedBy, locationAccordingTo, identifiedBy,
    references, license,
    key, identifier, gbifID
    ) 

indiv_citations <- lapply(1:nrow(theseDat), function(i) gbif_citation(theseDat$key[i])[[1]]$citation )

theseDat$citation <- unlist(indiv_citations)
  
write.csv(theseDat, file = file.path(wd$bin, "output_tidied_data.csv"))

big_citations <- lapply(theseDat$key, function(x) gbif_citation(x ) )

big_citations %>% 
  unique %>% 
  save(., file = file.path(wd$bin, "tidied_citations.Rdata"))
