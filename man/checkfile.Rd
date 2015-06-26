\name{checkfile}
\alias{checkfile}
\title{
Check the filename and directory.
}
\description{
There is the possibility to work with a given folder structure (CMSAF-R-TOOLBOX).
This function is only for internal use within other cmsaf-package functions.
To use this option the variable 'ptt' (path to toolbox) has to be defined.

The CM SAF R TOOLBOX consists of the 'cmsaf' R-package and a set of R-scripts, 
which are helpful to work with CM SAF netcdf data. There are R-scripts, which 
can be used to analyse and plot CM SAF netcdf data. The TOOLBOX includes also
R-scripts, which help unexperienced R-users to apply the functions of the cmsaf
R-package. The CM SAF R TOOLBOX is freely available on request and in the near 
future via the CM SAF homepage (www.cmsaf.eu).
}
\usage{
checkfile(infile, outfile)
}
\arguments{
  \item{infile}{
Filename of input netcdf file. This may include the directory.
}
  \item{outfile}{
Filename of output netcdf file. This may include the directory.
}
}
\value{
The infile name and outfile name are returned.
}
\author{
Steffen Kothe
}
