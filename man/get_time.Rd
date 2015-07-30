\name{get_time}
\alias{get_time}
\title{
Convert time steps to POSIXct.
}
\description{
Times in netcdf data are generally given in form of a time step and 
a time unit. This functions uses both information to convert them to
POSIXct time values. 
}
\usage{
get_time(time.unit,time.step)
}
\arguments{
  \item{time.unit}{
Time unit, which is conform to the CF convention (character).
}
  \item{time.step}{
Time steps in form of a numeric or integer vector.
}
}
\value{
Time in form of POSIXct is returned. Default time zone is UTC. 
}
\author{
Steffen Kothe
}
