get_time <- function(time.unit,time.step){
  
  stopifnot(is.character(time.unit))
  stopifnot(is.numeric(time.step) && !any(is.na(time.step)))
    
# convert time.unit to POSIXct
  
  t.unit <- unlist(strsplit(time.unit," since "))[1]
  ref.date <- unlist(strsplit(time.unit," since "))[2]
  ref.date <- as.POSIXct(ref.date,tz="UTC")

# get factor to convert time.step to seconds
# for months it is only an estimation for 30 days

  factor <- 0

  # check reference time unit
  if (t.unit=="minutes"|t.unit=="Minutes"|t.unit=="Mins"|t.unit=="Min"|t.unit=="min"|t.unit=="mins")(factor <- 60)
  if (t.unit=="seconds"|t.unit=="Seconds"|t.unit=="Secs"|t.unit=="Sec"|t.unit=="sec"|t.unit=="secs")(factor <- 1)
  if (t.unit=="Hours"|t.unit=="Hour"|t.unit=="hour"|t.unit=="hours")(factor <- 60*60)
  if (t.unit=="Days"|t.unit=="Day"|t.unit=="day"|t.unit=="days")(factor <- 24*60*60)
  if (t.unit=="Weeks"|t.unit=="Week"|t.unit=="week"|t.unit=="weeks")(factor <- 7*24*60*60)
  if (t.unit=="Months"|t.unit=="Month"|t.unit=="month"|t.unit=="months")(factor <- 30*24*60*60)
  if (factor==0)(stop(cat(paste("Error! Non-compliant time unit: ",t.unit,sep="")),"\n"))

# calculate times

  check <- ifelse((time.step*factor)<=.Machine$integer.max,FALSE,TRUE)
  times <- ref.date+(time.step*factor)

  if (sum(check)>0)(cat(paste("Some times exeed maximum integer value and may be wrong: ",times[check])))
  
  return(times)  
}