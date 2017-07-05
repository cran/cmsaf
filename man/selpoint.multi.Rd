\name{selpoint.multi}
\alias{selpoint.multi}
\title{
Extract data at multiple points.
}
\description{
This function extracts all data at given points. The points are given 
by a pair of vectors with longitude and latitude coordinates. The function 
will find the closest grid points to the given coordinates and extracts the 
data for these points. For each point a separate output file is written.
The output-files can be optional in NetCDF or csv. Input can be a single
NetCDF file (given by the infile attribute) or a bunch of NetCDF files
(given by the path and pattern attributes).
}
\usage{
selpoint.multi(var,infile,path,pattern,outpath,lon1,lat1,station_names,format,nc34)
}
\arguments{
  \item{var}{
Name of NetCDF variable (character).
}
  \item{infile}{
Filename of input NetCDF file. This may include the directory (character).
Infile is not needed if path and pattern are given.
}
  \item{path}{
Directory of input files (character).
Will not be used if infile is given.
}
  \item{pattern}{
Pattern that all desired files in the 'path' directory have in common (character).
}
  \item{outpath}{
Directory were output files will be stored (character).
}
  \item{lon1}{
Longitude vector of desired points (numeric vector). 
lon1 must have the same length as lat1. 
}
  \item{lat1}{
Latitude vector of desired points (numeric vector).
lat1 must have the same length as lon1.
}
  \item{station_names}{
Optional vector of names, which will be used for the output files (character vector).
station_names must have the same length as lon1 and lat1.
}
  \item{format}{
Intended output format. Options are 'nc' or 'csv'. Default is 'nc' (character).
}
  \item{nc34}{
NetCDF version of output file. If nc34=4 the output file will be in NetCDFv4 format (numeric).
Default output is NetCDFv3.
}
}
\value{
For each pair of longitude and latitude coordinates one separate NetCDF or csv file 
including the selected data is written. The csv-files are tested for use in Excel and 
include four coloumns (Time ; Data ; Longitude ; Latitude), which are seperated by ';'.
If station_names are defined, the output files will be named according to this vector.
Otherwise, the output files will be named as selpoint_longitude_latitude.format. 
Already existing files will be overwritten in case that station_names are given or
renamed (e.g., selpoint1_longitude_latitude.nc) in case that no station_names are given.
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
  time <- seq(as.Date('2000-01-01'), as.Date('2010-12-31'), 'month')
  origin <- as.Date('1983-01-01 00:00:00')
  time <- as.numeric(difftime(time,origin,units='hour'))
  data <- array(250:350,dim=c(21,21,132))

## create example NetCDF

   x <- ncdim_def(name='lon',units='degrees_east',vals=lon)
   y <- ncdim_def(name='lat',units='degrees_north',vals=lat)
   t <- ncdim_def(name='time',units='hours since 1983-01-01 00:00:00',
   vals=time,unlim=TRUE)
   var1 <- ncvar_def('SIS','W m-2',list(x,y,t),-1,prec='short')
   vars <- list(var1)
   ncnew <- nc_create('CMSAF_example_file.nc',vars)
   ncvar_put(ncnew,var1,data)
   ncatt_put(ncnew,'lon','standard_name','longitude',prec='text')
   ncatt_put(ncnew,'lat','standard_name','latitude',prec='text')
   nc_close(ncnew)

## Select two points of the example CM SAF NetCDF file and write the output 
## to a csv-file.
   selpoint.multi(var='SIS',infile='CMSAF_example_file.nc', 
   outpath=getwd(),lon1=c(8,9),lat1=c(48,49),station_names=c("A","B"),format='csv')
}
