\name{change_att}
\alias{change_att}
\title{
Change attributes of a NetCDF variable.
}
\description{
This function can change the name, standard_name, long_name, units, _FillValue
and missing_value of a variable. There is no separate outfile, thus use this
function with care. The values for v_name, s_name, l_name, u_name, F_val and
m_val are optional and will only be changed if they are given. If an attribute
is not defined yet, it is added by the function.
}
\usage{
change_att(var, infile, v_name, s_name, l_name, u_name, F_val, m_val, 
val_prec='double')
}
\arguments{
  \item{var}{
Name of NetCDF variable (character).
}
  \item{infile}{
Filename of input NetCDF file. This may include the directory (character).
}
  \item{v_name}{
New variable name (character).
}
  \item{s_name}{
New standard name (character).
}
  \item{l_name}{
New long name (character).
}
  \item{u_name}{
New units name (character).
}
  \item{F_val}{
New fill value (numeric).
}
 \item{m_val}{
New missing value (numeric).
}
 \item{val_prec}{
Precision of the FillValue and missing value (character). Default is double.
}
}
\details{
The function uses the ncdf4 package to change information of the infile.
}
\value{
The function change_att changes variable information within the infile NetCDF.
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

## create NetCDF

   x <- ncdim_def(name='lon',units='degrees_east',vals=lon)
   y <- ncdim_def(name='lat',units='degrees_north',vals=lat)
   t <- ncdim_def(name='time',units='hours since 1983-01-01 00:00:00',
   vals=time,unlim=TRUE)
   var1 <- ncvar_def('Data1','W m-2',list(x,y,t),-1,prec='short')
   vars <- list(var1)
   ncnew <- nc_create('CMSAF_example_file.nc',vars)
   ncvar_put(ncnew,var1,data)
   ncatt_put(ncnew,'lon','standard_name','longitude',prec='text')
   ncatt_put(ncnew,'lat','standard_name','latitude',prec='text')
   nc_close(ncnew)

## Change the variable and standard name of the example CM SAF NetCDF file:
   change_att('Data1','CMSAF_example_file.nc', v_name='SIS',
   s_name='surface_downwelling_shortwave_flux_in_air')
}
