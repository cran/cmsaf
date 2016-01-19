\name{selpoint}
\alias{selpoint}
\title{
Extract data at a given point.
}
\description{
This function extracts all data at a given point. A point is given 
by a pair of longitude and latitude coordinates. The function will find
the closest grid point to the given coordinates and extracts the data
for this point. The output-file can be optional in netcdf or csv.
The outfile is checked for the correct file extension.
}
\usage{
selpoint(var, infile, outfile, lon1, lat1, format)
}
\arguments{
  \item{var}{
Name of netcdf variable (character).
}
  \item{infile}{
Filename of input netcdf file. This may include the directory (character).
}
  \item{outfile}{
Filename of output netcdf file. This may include the directory (character).
}
  \item{lon1}{
Longitude of desired point (numeric). 
}
  \item{lat1}{
Latitude of desired point (numeric).
}
  \item{format}{
Intended output format. Options are "nc" or "csv". Default is "nc" (character).
}
}
\value{
A netcdf or csv file including the selected point is written. The csv-file is
tested for use in Excel and includes two coloumns (Time and Data), which are
seperated by ";".
}
\author{
Steffen Kothe
}
\examples{
## Create an example netcdf file with a similar structure
## as used by CM SAF. The file is created with the ncdf4 package.
## Alternatively example data can be freely downloaded here: https://wui.cmsaf.eu/

library(ncdf4)

## create some (non-realistic) example data

  lon <- seq(5,15,0.5)
  lat <- seq(45,55,0.5)
  time <- seq(as.Date("2000-01-01"), as.Date("2010-12-31"), "month")
  origin <- as.Date("1983-01-01 00:00:00")
  time <- as.numeric(difftime(time,origin,units="hour"))
  data <- array(250:350,dim=c(21,21,132))

## create example netcdf

   x <- ncdim_def(name="lon",units="degrees_east",vals=lon)
   y <- ncdim_def(name="lat",units="degrees_north",vals=lat)
   t <- ncdim_def(name="time",units="hours since 1983-01-01 00:00:00",
   vals=time,unlim=TRUE)
   var1 <- ncvar_def("SIS","W m-2",list(x,y,t),-1,prec="short")
   vars <- list(var1)
   ncnew <- nc_create("CMSAF_example_file.nc",vars)
   ncvar_put(ncnew,var1,data)
   ncatt_put(ncnew,"lon","standard_name","longitude",prec="text")
   ncatt_put(ncnew,"lat","standard_name","latitude",prec="text")
   nc_close(ncnew)

## Select a point of the example CM SAF netcdf file and write the output 
## to a csv-file.
   selpoint("SIS","CMSAF_example_file.nc", "CMSAF_example_file_selpoint.nc",
   8,48,"csv")
}
