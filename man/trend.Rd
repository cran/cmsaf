\name{trend}
\alias{trend}
\title{
Determine linear trends.
}
\description{
The function determines the trend from data of a single CM SAF NetCDF input 
file basing on a simple linear model. Depending on the file size, this function 
could be very time consuming, thus there are two available options. 
Option 1 (default) is using an apply approach and will read the whole data in
once. This option is quite fast, but requires enough memory. 
Option 2 is using the same calculation, but reads the data pixel by pixel,
which is very slow, but can also be applied for large data files, which would
not fit into the memory at once.
}
\usage{
trend(var, infile, outfile, option, nc34)
}
\arguments{
  \item{var}{
Name of NetCDF variable (character).
}
  \item{infile}{
Filename of input NetCDF file. This may include the directory (character). 
The file should include a time series of 2D fields to work in the desired way.
}
  \item{outfile}{
Filename of output NetCDF file. This may include the directory (character).
}
  \item{option}{
The way of data handling. Option = 1 is fast but memory consuming (default).
Option = 2 is slow, but needs much less memory. Input is either 1 or 2 (numeric).
}
  \item{nc34}{
NetCDF version of output file. If nc34=4 the output file will be in NetCDFv4 format (numeric).
Default output is NetCDFv3.
}
}
\value{
A NetCDF file including three data layers is written. One layer (trend1)
contains the linear trend multiplied by the number of time steps. In older
versions of the package (<= 1.7) the trend was given in the same way as trend1.
Another layer (trend2) contains just the calculated linear trend. An additional 
layer contains a measure for the significance of the calculated trends, which
was derived using the 95 % confidence interval. 
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

## Determine the trend of the example CM SAF NetCDF file and write the 
## output to a new file.
   trend('SIS','CMSAF_example_file.nc','CMSAF_example_file_trend.nc')
}
