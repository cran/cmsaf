get_time_bounds_doy <- function(times, doy) {
  time_bnds <- array(NA, dim=c(2,max(doy,na.rm=TRUE)))

  count <- 1
  for (j in seq_len(max(doy, na.rm=TRUE))){
    day_dummy <- which(doy==j)
    time_bnds[1,count] <- times[min(day_dummy)]
    time_bnds[2,count] <- times[max(day_dummy)]
    count <- count+1
  }

  return(time_bnds)
}
