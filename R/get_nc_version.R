get_nc_version <- function(nc34) {
  if (nc34 == 4) {
    nc_format <- TRUE
    compression <- 4
  } else {
    nc_format <- FALSE
    compression <- NA
  }

  return(list(force_v4 = nc_format, compression = compression))
}
