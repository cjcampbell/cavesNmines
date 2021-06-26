

url <- "https://daymet.ornl.gov/single-pixel/api/data?lat=35.9621&lon=-84.2916&vars=dayl,prcp,srad,swe,tmax,tmin,vp&years=1980"

download.file(url = url, destfile = "daymet/test", extra = "--content-disposition")


