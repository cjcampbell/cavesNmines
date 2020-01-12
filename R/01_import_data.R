
#### Get, Clean Data ###
dat <- lapply(list(5218544, 5218543, 2432341), function(x){
  my_search <- occ_search(
    taxonKey = x, return = 'data', fields = 'all', limit=100000)
  my_cit <- gbif_citation(my_search)
  save( my_cit, file = file.path(wd$bin, paste0("rgbif_citation_", x, ".Rdata") ) )
  return(my_search)
}) %>% 
  plyr::ldply()

save(dat, file = file.path(wd$data, "data_import.Rdata"))
