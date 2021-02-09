# remotes::install_github("privefl/bigreadr")
library(bigreadr)
source('~/cavesNmines/R/00_Setup.R')
mypath <- list.files(
  pattern = "occurrence.txt", 
  full.names = T, recursive = T,
  file.path("/Users/cjcampbell", "BigZaddyData", "GBIF_20July2020_Chiroptera")
)
nlines(mypath)

mdf_filtered <- big_fread1(mypath, every_nlines = 4e5, .transform = function(df) {
  dplyr::filter(df, genus %in% c("Lasiurus", "Lasionycteris", "Aeorestes"))
})

targetDir <- file.path("/Users/cjcampbell", "BigZaddyData", "GBIF_20July2020_filtered_Lasiurus_Lasionycteris")
if(!dir.exists(targetDir)) dir.create(targetDir)

write.csv(mdf_filtered, file = file.path(targetDir, "occurrences.csv"))


