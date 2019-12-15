
#### Get, Clean Data ###
dat <- lapply(list(5218544, 5218543, 2432341), function(x){
  occ_search(taxonKey = x, return = 'data', fields = 'all', limit=10000)
}) %>% 
  plyr::ldply()

save(dat, file = file.path(wd$data, "data_import.Rdata"))
