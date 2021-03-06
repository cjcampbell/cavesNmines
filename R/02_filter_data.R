
# Import data
dat <- read.csv( file.path(targetDir, "occurrences.csv") )

# Specify key words and terms.
mySearchTerms <- c(
  # English
  "cave", "mine", "grotto", "quarry", "underground", "subterranean", "cavern", "tunnel","pit",
  # French
  "grotte", "mienne", "grotte", "carrière", "souterraine", "caverne", "tunnel", "fosse",
  # Spanish
  "cueva", "mina", "gruta", "cantera", "subterráneo", "caverna", "túnel", "pozo"
)

# # New, better way:
# searchterms <- lapply(mySearchTerms, function(i) paste0("\\<", i, "\\>")) # search for whole words
# 
# dat %>% 
#   dplyr::filter( basisOfRecord %in% c("FOSSIL_SPECIMEN", "UKNOWN") == FALSE) %>% 
#   filter_all(any_vars(grepl(paste(mySearchTerms, collapse="|"),.))) 

# Columns to keep regardless of match
cols2Keep <- c(
  "species", "rightsHolder", "identifier", "recordedBy", "institutionCode",
  "stateProvince", "countryCode",
  "year", "month", "day"
  )

myMatches <- lapply(mySearchTerms,  function(term){
  row_nums <- sapply(1:ncol(dat), function(x) grep(paste0("\\b", term, "\\b"), dat[,x]) ) %>% 
    unlist
  # col_nums <- sapply(1:nrow(dat), function(y) grep(term, dat[y,]) ) %>% 
  #   unlist 
  return(list(row_nums,# col_nums, 
              term))
})

save(myMatches, file = file.path(wd$bin, "myMatches.Rdata"))
 
# Select target rows, add identifier indicating which column has which key word
dat <- dat %>% mutate(codeword = NA
                      #, colWithCodeword = NA
                      )
for(a in 1:length(myMatches)){
  dat[  myMatches[[a]][[1]] , "codeword"]        <-  myMatches[[a]][[2]]#myMatches[[a]][[3]]
  #dat[  myMatches[[a]][[1]] , "colWithCodeword"] <- colnames(dat)[ myMatches[[a]][[2]] ]
}

# # Bring content of colWithCodeword into new column
# tidyDat0 <- dat %>%  dplyr::filter(!is.na(codeword))
# for(b in 1:nrow(tidyDat0)){
#   tidyDat0[ b , tidyDat0[ b , "colWithCodeword"] ]
#   
# }
# 

tidyDat <- dat %>% 
  #dplyr::select( append(cols2Keep, c("codeword", "colWithCodeword" ) )) %>% 
  dplyr::filter(!is.na(codeword)) %>% 
  mutate_if(is.character, list(~na_if(., "<NA>"))) %>% 
  select_if(~sum(!is.na(.)) > 0) # remove columns with only NA

save(tidyDat, file = file.path(wd$bin, "tidyDat.Rdata"))
write.csv(tidyDat, file = file.path(wd$bin, "tidyDat.csv"))
