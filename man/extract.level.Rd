\name{extract.level}
\alias{extract.level}
\title{
Extract levels from 4-dimensional NetCDF files.
}
\description{
This function extracts one or all levels of a 4-dimensional NetCDF file.
A level is defined as a dimension, which does not correspond to longitude,
latitude or time. The user can choose either one specific level (given by
an integer) or all levels (level = "all").  
}
\usage{
extract.level(var, infile, outfile, level, nc34)
}
\arguments{
  \item{var}{
Name of NetCDF variable (character).
}
  \item{infile}{
Filename of input NetCDF file. This may include the directory (character).
}
  \item{outfile}{
Filename of output NetCDF file. This may include the directory (character).
}
  \item{level}{
Number of level (default = 1) or all levels (level = "all") (numeric or character).
}
  \item{nc34}{
NetCDF version of output file. If nc34=4 the output file will be in NetCDFv4 format (numeric).
Default output is NetCDFv3.
}
}
\value{
A NetCDF file including the selected level is written. In case of level = "all"
all levels are written in separate NetCDF files and outfile names are expanded
by "_level" and the level number.
}
\author{
Steffen Kothe
}
\examples{
## Create an example NetCDF file with a similar structure
## as used by CM SAF. The file is created with the ncdf4 package.
## Alternatively example data can be freely downloaded here: 
## <https://wui.cmsaf.eu/>

library(ncdf4)

## create some (non-realistic) example data

  lon <- seq(5,15,0.5)
  lat <- seq(45,55,0.5)
  height <- seq(0,1000,100)
  time <- seq(as.Date('2000-01-01'), as.Date('2010-12-31'), 'month')
  origin <- as.Date('1983-01-01 00:00:00')
  time <- as.numeric(difftime(time,origin,units='hour'))
  data <- array(250:350,dim=c(21,21,11,132))

## create example NetCDF

   x <- ncdim_def(name='lon',units='degrees_east',vals=lon)
   y <- ncdim_def(name='lat',units='degrees_north',vals=lat)
   z <- ncdim_def(name='height',units='m',vals=height)
   t <- ncdim_def(name='time',units='hours since 1983-01-01 00:00:00',
   vals=time,unlim=TRUE)
   var1 <- ncvar_def('SIS','W m-2',list(x,y,z,t),-1,prec='short')
   vars <- list(var1)
   ncnew <- nc_create('CMSAF_example_file.nc',vars)
   ncvar_put(ncnew,var1,data)
   ncatt_put(ncnew,'lon','standard_name','longitude',prec='text')
   ncatt_put(ncnew,'lat','standard_name','latitude',prec='text')
   ncatt_put(ncnew,'height','standard_name','height',prec='text') 
   nc_close(ncnew)

## Extract the first level of the example CM SAF NetCDF file and write 
## the output to a new file.
   extract.level('SIS','CMSAF_example_file.nc', 
   'CMSAF_example_file_extract.level.nc')
## Extract all levels of the example CM SAF NetCDF file and write 
## the output to a new file.
   extract.level('SIS','CMSAF_example_file.nc', 
   'CMSAF_example_file_extract.level.nc',"all")
}
