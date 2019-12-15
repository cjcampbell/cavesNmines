
# Load packages.
library(rgbif)

# Make an object to help navigate the subdirectories.
wd <- list()
wd$bin  <- file.path( getwd(), "bin" )
wd$data <- file.path( getwd(), "data" )
wd$R    <- file.path( getwd(), "R" )
wd$figs <- file.path( getwd(), "figs" )
