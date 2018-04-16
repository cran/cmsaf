cmsaf.sub <-
function(vari1,vari2,infile1,infile2,outfile,nc34=3){

  start.time <- Sys.time()

# check filename

  filecheck1 <- checkfile(infile1,outfile)
  filecheck2 <- checkfile(infile2,outfile)

  if (filecheck1[[1]]&filecheck2[[1]]){
    infile1 <- filecheck1[[2]]
    infile2 <- filecheck2[[2]]
    outfile <- filecheck1[[3]]   

# define standard names

   t_name <- "time"
   t_standard_name = "time"
   t_units = "undefined"
   t_calendar = "undefined"

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
   v_long_name = paste("Result of sub-function: ",vari1,"-",vari2,sep="")
   v_units = "undefined"
   v__FillValue = "undefined"
   v_missing_value = "undefined"

   info = "Created with the CM SAF R Toolbox."
   var_prec="float"

   att_list <- c("standard_name","units","_FillValue","missing_value","calendar")
   v_att_list <- c("v_standard_name","v_units","v__FillValue","v_missing_value","v_calendar")
  
# get file information of infile1

  cat("get file information of infile1", "\n")

  id <- nc_open(infile1)

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
  var_default <- subset(varnames, !(varnames %in% c("lat","lon","time_bnds","nb2","time")))
  
  if (toupper(vari1) %in% toupper(var_default)){
    vari1 <- var_default[which(toupper(vari1)==toupper(var_default))]
  } else {
      cat("Variable ",vari1," not found.",sep="","\n")
      vari1 <- var_default[1]
      cat("Variable ",vari1," will be used.",sep="","\n")
    }
  
   # get variable precision 
    varind    <- which(varnames==vari1)
    varprec   <- NULL
    var_prec1 <- NULL
    varprec   <- id$var[[varind]]$prec
    if (!is.null(varprec)){
      if (varprec %in% c("short", "float", "double", "integer", "char", "byte")){
        (var_prec1 <- varprec)
      }
    }

   if (vari1 %in% varnames){
    for (i in 1:length(att_list)){
      att_dum <- ncatt_get(id,vari1,att_list[i])
      if (att_dum$hasatt){
	      assign(v_att_list[i],att_dum$value)}
    }

    # get data of first file

	  lon <- ncvar_get(id,lon_name)
	  lat <- ncvar_get(id,lat_name)
	  time1 <- ncvar_get(id,t_name)
	  time_len <- length(time1)
   } else {
      nc_close(id)
      stop(cat(paste("Variable ",vari1," not found! File contains: ",varnames,sep="")),"\n")}

      # calculate field maximum 
	
	 maxval <- array(NA,dim=c(3))
	 if (time_len>=3){
	   samp <- sample(c(1:time_len),3)
	 } else {
	   samp <- 1
	 }
	 count <- 1
	 for (i in samp){
	   data1 <- ncvar_get(id,vari1,start=c(1,1,i),count=c(-1,-1,1))
	   maxval[count] <- max(data1,na.rm=T)
	   count <- count+1
	 }
 	
	 if (v__FillValue == "undefined"){ 
	     v__FillValue = v_missing_value}
	 if (v_missing_value == "undefined"){ 
	     v_missing_value = v__FillValue}
	
	  # check max to avoid problems with fillvalue
	
	  fval <- c(-99,-999,-9999)
	  maxval <- max(maxval,na.rm=TRUE)
	  maxval <- abs(maxval)*(-2.5)
	  dum <- min(which(fval<maxval,arr.ind=TRUE),na.rm=TRUE)
	  mval <- fval[dum]
	  v__FillValue = mval
	  v_missing_value = mval   

  nc_close(id)

  # get time information of infile2

  cat("get file information of infile2", "\n")

  id <- nc_open(infile2)

  # get information about variables
	
  varnames2 <- names(id$var)
  var_default <- subset(varnames2, !(varnames2 %in% c("lat","lon","time_bnds","nb2","time")))
  
  if (toupper(vari2) %in% toupper(var_default)){
    vari2 <- var_default[which(toupper(vari2)==toupper(var_default))]
  } else {
      cat("Variable ",vari2," not found.",sep="","\n")
      vari2 <- var_default[1]
      cat("Variable ",vari2," will be used.",sep="","\n")
    }
  
   # get variable precision 
    varind    <- which(varnames2==vari2)
    varprec   <- NULL
    var_prec2 <- NULL
    varprec   <- id$var[[varind]]$prec
    if (!is.null(varprec)){
      if (varprec %in% c("short", "float", "double", "integer", "char", "byte")){
        (var_prec2 <- varprec)
      }
    }

   if (vari2 %in% varnames2){

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
      
      # get data of second file

	  lon2 <- ncvar_get(id,lon_name)
	  lat2 <- ncvar_get(id,lat_name)
	  time2 <- ncvar_get(id,t_name)
	  time_len2 <- length(time2)
   }else{
      nc_close(id)
      stop(cat(paste("Variable ",vari2," not found! File contains: ",varnames,sep="")),"\n")}

  nc_close(id)
  
  # define variable precision
  
  if(!is.null(var_prec1)&!is.null(var_prec2)){
    prec <- c("short", "integer", "float", "double", "char", "byte")
    var_prec <- prec[max(c(which(var_prec1==prec),which(var_prec2==prec)))]
  }

  # check dimensions of infile1 and infile2

  case=0

  if (length(lon)==length(lon2)&length(lat)==length(lat2)){
    if (time_len==time_len2&time_len>1) case=1
    if (time_len==1&time_len2==1) 	case=2
    if (time_len==1&time_len2>1) 	case=3
    if (time_len>1&time_len2==1)	case=4
  } else{cat("Dimensions of infiles do not match!", "\n")} 

  if (case!=0){

    target <- array(NA,dim=c(length(lon),length(lat),1))

    if (case==1) time=time1
    if (case==2) time=time1
    if (case==3) time=time2
    if (case==4) time=time1

 # create netcdf

  cat("create netcdf", "\n")

  # NetCDF format 3 or 4
  
  if (nc34==4){
    nc_format <- as.logical(1)
    compression = 4
  } else {
    nc_format <- as.logical(0)
    compression = NA
  }

    target[is.na(target)] <- v_missing_value

    x <- ncdim_def(name="lon",units=lon_units,vals=lon)
    y <- ncdim_def(name="lat",units=lat_units,vals=lat)
    t <- ncdim_def(name="time",units=t_units,vals=time[1],unlim=TRUE)

    var1 <- ncvar_def(name=vari1,units=v_units,dim=list(x,y,t),missval=v_missing_value,
                      prec=var_prec,compression=compression)

      vars <- list(var1)
      ncnew <- nc_create(outfile,vars,force_v4=nc_format)

      ncvar_put(ncnew,var1,target)

      ncatt_put(ncnew,vari1,"standard_name",v_standard_name,prec="text")
      ncatt_put(ncnew,vari1,"long_name",v_long_name,prec="text")

      ncatt_put(ncnew,"time","standard_name",t_standard_name,prec="text")
      ncatt_put(ncnew,"time","calendar",t_calendar,prec="text")

      ncatt_put(ncnew,"lon","standard_name",lon_standard_name,prec="text")
      ncatt_put(ncnew,"lon","long_name",lon_long_name,prec="text")
      ncatt_put(ncnew,"lon","axis",lon_axis,prec="text")

      ncatt_put(ncnew,"lat","standard_name",lat_standard_name,prec="text")
      ncatt_put(ncnew,"lat","long_name",lat_long_name,prec="text")
      ncatt_put(ncnew,"lat","axis",lat_axis,prec="text")

      ncatt_put(ncnew,0,"Info",info,prec="text")

    # get data of infile1 and infile2 and subtract corresponding fields

    if (case==1){
      id1 <- nc_open(infile1)
      id2 <- nc_open(infile2)
  
      for (i in 1:length(time)){
	      dum_dat1 <- ncvar_get(id1,vari1,start=c(1,1,i),count=c(-1,-1,1))
	      dum_dat2 <- ncvar_get(id2,vari2,start=c(1,1,i),count=c(-1,-1,1))
	      cat("\r","sub fields ",i," of ",length(time),sep="")
	      dum_data <- dum_dat1-dum_dat2
	      dum_data[is.na(dum_data)] <- v_missing_value
	      ncvar_put(ncnew,var1,dum_data,start=c(1,1,i),count=c(-1,-1,1))
	      ncvar_put(ncnew,t,time[i], start=i, count=1)
      }
      nc_close(id1)
      nc_close(id2)
      nc_close(ncnew)
    }

     if (case==2){
      id1 <- nc_open(infile1)
      id2 <- nc_open(infile2)
      dum_dat1 <- ncvar_get(id1,vari1,start=c(1,1,1),count=c(-1,-1,1))
      dum_dat2 <- ncvar_get(id2,vari2,start=c(1,1,1),count=c(-1,-1,1))
      cat("\r","sub fields",sep="")
      dum_data <- dum_dat1-dum_dat2
      dum_data[is.na(dum_data)] <- v_missing_value
      ncvar_put(ncnew,var1,dum_data,start=c(1,1,1),count=c(-1,-1,1))
      ncvar_put(ncnew,t,time[1], start=1, count=1)
      nc_close(id1)
      nc_close(id2)
      nc_close(ncnew)
    }

    if (case==3){
      id1 <- nc_open(infile1)
      id2 <- nc_open(infile2)
      dum_dat1 <- ncvar_get(id1,vari1,start=c(1,1,1),count=c(-1,-1,1))
  
      for (i in 1:length(time)){	
	      dum_dat2 <- ncvar_get(id2,vari2,start=c(1,1,i),count=c(-1,-1,1))
	      cat("\r","sub fields ",i," of ",length(time),sep="")
	      dum_data <- dum_dat1-dum_dat2
	      dum_data[is.na(dum_data)] <- v_missing_value
	      ncvar_put(ncnew,var1,dum_data,start=c(1,1,i),count=c(-1,-1,1))
	      ncvar_put(ncnew,t,time[i], start=i, count=1)
      }
      nc_close(id1)
      nc_close(id2)
      nc_close(ncnew)
    }

    if (case==4){
      id1 <- nc_open(infile1)
      id2 <- nc_open(infile2)
      dum_dat2 <- ncvar_get(id2,vari2,start=c(1,1,1),count=c(-1,-1,1))
  
      for (i in 1:length(time)){	
	      dum_dat1 <- ncvar_get(id1,vari1,start=c(1,1,i),count=c(-1,-1,1))
	      cat("\r","sub fields ",i," of ",length(time),sep="")
	      dum_data <- dum_dat1-dum_dat2
	      dum_data[is.na(dum_data)] <- v_missing_value
	      ncvar_put(ncnew,var1,dum_data,start=c(1,1,i),count=c(-1,-1,1))
	      ncvar_put(ncnew,t,time[i], start=i, count=1)
      }
      nc_close(id1)
      nc_close(id2)
      nc_close(ncnew)
    }

  end.time <- Sys.time()
  cat("\n","processing time: ",round(as.numeric(end.time-start.time,units="secs"),digits=2)," s",sep="", "\n")
  } # end if case!=0
  } # endif filecheck
}
