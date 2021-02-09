
# Load packages.
library(plyr)
library(tidyverse)
library(stringr)
library(data.table)

mywd <- "/Users/cjcampbell/cavesNmines"
# Make an object to help navigate the subdirectories.
wd <- list()
wd$bin  <- file.path( mywd, "bin" )
wd$data <- file.path( mywd, "data" )
wd$R    <- file.path( mywd, "R" )
wd$figs <- file.path( mywd, "figs" )
wd$tmp <- file.path(wd$bin, "tmp")

lapply(wd, function(i) {
  if(!dir.exists(i)) { dir.create(i) }
})
