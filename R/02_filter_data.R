
# Import data
load(file.path(wd$data, "data_import.Rdata"))

# Columns to keep regardless of match
cols2Keep <- c(
  "species", "rightsHolder", "identifier", "recordedBy", "institutionCode",
  "stateProvince", "countryCode",
  "year", "month", "day"
  )

# Specify key words and terms.
searchterms <- c("cave", "mine", "building", "wall", "house", "dwelling", "grotto", "well", "pit", "quarry","grotte")

myMatches <- lapply(searchterms,  function(term){
  row_nums <- sapply(1:ncol(dat), function(x) grep(term, dat[,x]) ) %>% 
    unlist %>% 
    unique %>% 
    sort
  col_nums <- sapply(1:nrow(dat), function(y) grep(term, dat[y,]) ) %>% 
    unlist %>% 
    unique %>% 
    sort
  return(list(row_nums, col_nums, term))
})

save(myMatches, file = file.path(wd$bin, "myMatches.Rdata"))

