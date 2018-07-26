read_ncvar <-
function(var,infile){

  start.time <- Sys.time()

# check filename

  filecheck <- checkfile(infile,"outfile")

  if (filecheck[[1]]){
    infile <- filecheck[[2]]   

# define standard names of variables and dimensions

   t_name <- "time"
   t_standard_name = "time"
   t_units = "undefined"
   t_calendar = "undefined"

   nb2_units = "1"

   lat_name = "latitude"
   lat_standard_name = "latitude"
   lat_long_name = "latitude"
   lat_units = "degrees_north"
   lat_axis = "Y"

   lon_name = "longitude"
   lon_standard_name = "longitude"
   lon_long_name = "longitude"
   lon_units = "degrees_east"
   lon_axis = "X"

   v_standard_name = "undefined"
   v_long_name = "undefined"
   v_units = "undefined"
   v__FillValue = "undefined"
   v_missing_value = "undefined"

   info = "Created with the CM SAF R Toolbox." 
   var_prec="float"

   att_list <- c("standard_name","long_name","units","_FillValue","missing_value","calendar")
   v_att_list <- c("v_standard_name","v_long_name","v_units","v__FillValue","v_missing_value","v_calendar")
  
# get file information

  cat("get file information", "\n")

  id <- nc_open(infile)

  # get information about dimensions

  dimnames <- names(id$dim)

 # check standard_names of dimensions
    for (i in 1:length(dimnames)){
	    sn <- ncatt_get(id,dimnames[i],"standard_name")
	    ln <- ncatt_get(id,dimnames[i],"long_name")
	    if (!is.null(sn$hasatt)){
	      if (sn$hasatt){
	        sn <- sn$value
	        if (sn %in% c("longitude","Longitude","Lon","lon"))(lon_name <- dimnames[i])
	        if (sn %in% c("latitude","Latitude","Lat","lat"))(lat_name <- dimnames[i])
	        if (sn=="time"|sn=="Time")(t_name <- dimnames[i])
	      } else {
	          if (ln$hasatt){
	            ln <- ln$value
	            if (ln %in% c("longitude","Longitude","Lon","lon"))(lon_name <- dimnames[i])
	            if (ln %in% c("latitude","Latitude","Lat","lat"))(lat_name <- dimnames[i])
	            if (ln=="time"|ln=="Time")(t_name <- dimnames[i])
	          }
	       }
	    }
    }

    for (i in 1:length(dimnames)){
      if (t_name %in% dimnames){
        attnames <- names(id$dim[[i]])
        if ("units" %in% attnames){
	        t_units <- ncatt_get(id,t_name,"units")$value}
        if ("calendar" %in% attnames){
	        t_calendar <- ncatt_get(id,t_name,"calendar")$value}
      }
    }

  # get information about variables
	
  varnames <- names(id$var)
  varnames <- append(varnames,dimnames)
  var_default <- subset(varnames, !(varnames %in% c("time_bnds","nb2")))
  
  if (toupper(var) %in% toupper(var_default)){
    var <- var_default[which(toupper(var)==toupper(var_default))]
  } else {
      cat("Variable ",var," not found.",sep="","\n")
      var <- var_default[1]
      cat("Variable ",var," will be used.",sep="","\n")
    }

   if (var %in% varnames){
    for (i in 1:6){
      att_dum <- ncatt_get(id,var,att_list[i])
      if (att_dum$hasatt){
	      assign(v_att_list[i],att_dum$value)}
    }

    # get details of file
    data  <- ncvar_get(id,var)
	  time1 <- ncvar_get(id,t_name)
	  time_len <- length(time1)
   }else{
      nc_close(id)
      stop(cat(paste("Variable ",var," not found! File contains: ",varnames,sep="")),"\n")}

	 if (v__FillValue == "undefined"){ 
	     v__FillValue = v_missing_value}
	 if (v_missing_value == "undefined"){ 
	     v_missing_value = v__FillValue}

  nc_close(id)   

# extract time information

  date.time <- get_time(t_units,time1)

  # create return list
  if(length(dim(data))>=3 & "TIME" %in% toupper(dimnames)){
    output <- list(data,date.time)
    names(output) <- c(var,t_name)
  } else {
    output <- list(data)
    names(output) <- c(var)
  }
  
  cat(paste("list with ",length(output)," element(s) returned",sep=""), "\n")
  return(output)
  
  end.time <- Sys.time()
  } # endif filecheck
}
