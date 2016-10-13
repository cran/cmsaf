\name{yseasmax}
\alias{yseasmax}
\title{
Determine multi-year seasonal maxima.
}
\description{
The function determines multi-year seasonal maximum values from data of a single 
CM SAF netcdf input file.
}
\usage{
yseasmax(var, infile, outfile)
}
\arguments{
  \item{var}{
Name of netcdf variable (character).
}
  \item{infile}{
Filename of input netcdf file. This may include the directory (character).
}
  \item{outfile}{
Filename of output netcdf file. This may include the directory (character)..
}
}
\value{
A netcdf file including a timeseries of multi-year seasonal maxima is written.
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

## Determine the multi-year seasonal maximum of the example CM SAF netcdf 
## file and write the output to a new file.
   yseasmax("SIS","CMSAF_example_file.nc", "CMSAF_example_file_yseasmax.nc")
}