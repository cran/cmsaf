selpoint.multi <-
function(var,infile,path,pattern,outpath,lon1,lat1,station_names,format="nc",nc34=3){

  start.time <- Sys.time()

# check function arguments
  
  case <- 0
  
  if (!missing(infile)){
    case <- 1
    if (!missing(path)) (cat("infile defined, path and pattern will not be used","\n"))
  }
  
  if (missing(infile)&!missing(path)&!missing(pattern)){
    case <- 2
  }
  
  if (length(lon1)!=length(lat1)) {
      stop(cat("Length of longitude vector has to be equal to length of latitude vector!","\n"))}

# check filename

  if (case==1){ 
    filecheck <- checkfile(infile,NA)
    if (filecheck[[1]]){
      infile <- filecheck[[2]]
    } else { case <- 0}
  }

if (case!=0){
  if (case==1){

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
  var_default <- subset(varnames, !(varnames %in% c("lat","lon","time_bnds","nb2","time")))
  
  if (toupper(var) %in% toupper(var_default)){
    var <- var_default[which(toupper(var)==toupper(var_default))]
  } else {
      cat("Variable ",var," not found.",sep="","\n")
      var <- var_default[1]
      cat("Variable ",var," will be used.",sep="","\n")
    }
  
    # set variable precision 
    varind   <- which(varnames==var)
    varprec  <- NULL
    varprec  <- id$var[[varind]]$prec
    if (!is.null(varprec)){
      if (varprec %in% c("short", "float", "double", "integer", "char", "byte")){
        (var_prec <- varprec)
      }
    }

   if (var %in% varnames){
    for (i in 1:6){
      att_dum <- ncatt_get(id,var,att_list[i])
      if (att_dum$hasatt){
	      assign(v_att_list[i],att_dum$value)}
    }
   } else {
      nc_close(id)
      stop(cat(paste("Variable ",var," not found! File contains: ",varnames,sep="")),"\n")}

  # get data of infile

	lon <- ncvar_get(id,lon_name)
	lat <- ncvar_get(id,lat_name)
	time1 <- ncvar_get(id,t_name)
	time_len <- length(time1)

  # find closest point to target coordinates using sp package

	cat("find closest point(s) to target coordinates", "\n")
	
	target_lon  <- NULL
	target_lat  <- NULL
  target_data <- NULL
  
	dlon <- abs(lon[1]-lon[2])
	dlat <- abs(lat[1]-lat[2])
	
	for (n in 1:length(lon1)){
	  lon_limit <- which(lon>=(lon1[n]-dlon)&lon<=(lon1[n]+dlon))  
	  lat_limit <- which(lat>=(lat1[n]-dlat)&lat<=(lat1[n]+dlat)) 

    if (exists("lon_limit")&exists("lat_limit")){
  
	  lon2 <- lon[lon_limit]
	  lat2 <- lat[lat_limit]

	  pos <- SpatialPoints(cbind(lon1[n],lat1[n]), proj4string=CRS("+proj=longlat +datum=WGS84"))
	  dum_dist <- 1000
	  for (i in 1:length(lon2)){
	    for (j in 1:length(lat2)){
	      dist <- spDistsN1(pos, c(lon2[i],lat2[j]), longlat = FALSE)
	      if (dist<=dum_dist){
		      dum_dist <- dist
		      dumi <- i
		      dumj <- j
	      }
	    }
	  }

	  lon_limit <- which(lon==lon2[dumi])  
	  lat_limit <- which(lat==lat2[dumj])
    }

    if (exists("lon_limit")&exists("lat_limit")){

	  target_lon <- append(target_lon,lon[lon_limit])
	  target_lat <- append(target_lat,lat[lat_limit])
	
	  if (var %in% varnames){
	    data1 <- ncvar_get(id,var,start=c(lon_limit,lat_limit,1),count=c(1,1,-1))
	  }
	  target_data <- rbind(target_data,data1)
   } else {cat("WARNING! Coordinates outside of the domain.", "\n")}
	  
	} # end for n

    if (v__FillValue == "undefined"){ 
      v__FillValue = v_missing_value}
    if (v_missing_value == "undefined"){ 
      v_missing_value = v__FillValue}
	
	  tbnds <- NULL   
	  if ("time_bnds" %in% varnames){
      tbnds1 <- ncvar_get(id,"time_bnds")
      tbnds <- rbind(tbnds,tbnds1)
    }

    nc_close(id)

  if (!is.null(target_data)){
    if (length(time1)==1){
      dummy <- array(NA,dim=c(1,1,1))
      dummy[1,1,1] <- target_data[1,]
      data1 <- dummy
    }
  }
  
   target_lon <- round(target_lon,digits=3)
   target_lat <- round(target_lat,digits=3)
   
 } # end if case 1 ___________________________________
  
 if (case==2){
   # define standard names of variables and dimensions

   t_name <- "time"
   t_standard_name = "time"
   t_units = "undefined"
   t_calendar = "standard"

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

   info = "Created with the CM SAF R toolbox." 
   var_prec="float"

   att_list <- c("standard_name","long_name","units","_FillValue","missing_value","calendar")
   v_att_list <- c("v_standard_name","v_long_name","v_units","v__FillValue","v_missing_value","v_calendar")
  
# get file information

  cat("get file information", "\n")

  filelist <- list.files(path=path, pattern=pattern)
  filelist <- sort(filelist)
  fdim <- length(filelist)

  file=filelist[1]
  file <- file.path(path,file)
  id <- nc_open(file)

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
  
    if (toupper(var) %in% toupper(var_default)){
      var <- var_default[which(toupper(var)==toupper(var_default))]
    } else {
        cat("Variable ",var," not found.",sep="","\n")
        var <- var_default[1]
        cat("Variable ",var," will be used.",sep="","\n")
      }
    
  if (var %in% varnames){
  
    # set variable precision 
    varind   <- which(varnames==var)
    varprec  <- NULL
    varprec  <- id$var[[varind]]$prec
    if (!is.null(varprec)){
      if (varprec %in% c("short", "float", "double", "integer", "char", "byte")){
        (var_prec <- varprec)
      }
    }
    
    for (i in 1:6){
      att_dum <- ncatt_get(id,var,att_list[i])
      if (att_dum$hasatt){
	      assign(v_att_list[i],att_dum$value)}
    }

      # get data of first file and cut desired region

	  lon <- ncvar_get(id,lon_name)
	  lat <- ncvar_get(id,lat_name)
	  time1 <- ncvar_get(id,t_name)
	  time_len <- length(time1)

	  # find closest point to target coordinates using sp package

	  cat("find closest point(s) to target coordinates", "\n")
	
	  target_lon  <- NULL
	  target_lat  <- NULL
	  index_x     <- NULL
	  index_y     <- NULL

	  dlon <- abs(lon[1]-lon[2])
	  dlat <- abs(lat[1]-lat[2])
	
	  for (n in 1:length(lon1)){
	    lon_limit <- which(lon>=(lon1[n]-dlon)&lon<=(lon1[n]+dlon))  
	    lat_limit <- which(lat>=(lat1[n]-dlat)&lat<=(lat1[n]+dlat)) 

      if (exists("lon_limit")&exists("lat_limit")){
  
	      lon2 <- lon[lon_limit]
	      lat2 <- lat[lat_limit]

	      pos <- SpatialPoints(cbind(lon1[n],lat1[n]), proj4string=CRS("+proj=longlat +datum=WGS84"))
	      dum_dist <- 1000
	      for (i in 1:length(lon2)){
	        for (j in 1:length(lat2)){
	          dist <- spDistsN1(pos, c(lon2[i],lat2[j]), longlat = FALSE)
	          if (dist<=dum_dist){
		          dum_dist <- dist
		          dumi <- i
		          dumj <- j
	          }
	        }
	      }

	      lon_limit <- which(lon==lon2[dumi])  
	      lat_limit <- which(lat==lat2[dumj])
      }

      if (exists("lon_limit")&exists("lat_limit")){

	    target_lon <- append(target_lon,lon[lon_limit])
	    target_lat <- append(target_lat,lat[lat_limit])
	    index_x    <- append(index_x,lon_limit)
	    index_y    <- append(index_y,lat_limit)

      } else {cat("WARNING! Coordinates outside of the domain.", "\n")}
	  
    } # end for n
  } else {
     nc_close(id)
     stop(cat(paste("Variable ",var," not found! File contains: ",varnames,sep="")),"\n")}

    if (v__FillValue == "undefined"){ 
      v__FillValue = v_missing_value}
    if (v_missing_value == "undefined"){ 
      v_missing_value = v__FillValue}

    nc_close(id)

    # store data in target field and check time order
    
    target_data <- NULL
    time_sorting <- NULL
    tbnds <- NULL
    if (var %in% varnames){
      for (j in 1:fdim){
        cat("\r","Read file ",j," of ",fdim,sep="")
        dummy <- NULL
        file=filelist[j]
        file <- paste(path,"/",file,sep="")
        id <- nc_open(file)
	      for (i in 1:length(index_x)){
	        dummy <- cbind(dummy,ncvar_get(id,var,start=c(index_x[i],index_y[i],1),count=c(1,1,-1)))
	      }
        dum_time <- as.numeric(ncvar_get(id,t_name))
        time_sorting <- append(time_sorting,dum_time)
        target_data <- rbind(target_data,dummy)
        if ("time_bnds" %in% varnames){
          tbnds1 <- ncvar_get(id,"time_bnds")
          tbnds <- rbind(tbnds,tbnds1)
        }
        nc_close(id)
      }
    }
    
    cat("\n")
    time1 <- time_sorting
    target_data <- aperm(target_data,c(2,1))
    target_data <- target_data[,order(time_sorting)]
    
    target_lon <- round(target_lon,digits=3)
    target_lat <- round(target_lat,digits=3)

  # get time reference

  dt_ref <- get_time(t_units,0)
  unit_ref <- unlist(strsplit(t_units,split=" "))[1]

  # check reference time unit
  if (unit_ref=="minutes"|unit_ref=="Minutes"|unit_ref=="Mins"|unit_ref=="Min"|unit_ref=="min")(unit_ref <- "mins")
  if (unit_ref=="seconds"|unit_ref=="Seconds"|unit_ref=="Secs"|unit_ref=="Sec"|unit_ref=="sec")(unit_ref <- "secs")
  if (unit_ref=="Hours"|unit_ref=="Hour"|unit_ref=="hour")(unit_ref <- "hours")
  if (unit_ref=="Days"|unit_ref=="Day"|unit_ref=="day")(unit_ref <- "days")
  if (unit_ref=="Months"|unit_ref=="Month"|unit_ref=="month")(unit_ref <- "months")
  if (unit_ref!="mins"&unit_ref!="secs"&unit_ref!="hours"&unit_ref!="days"&unit_ref!="weeks"&unit_ref!="months")(unit_ref <- "auto")

 } # end case==2 _____________________________________
  
# file output  

  if (!is.null(target_data)){
    if (is.null(dim(target_data)[1]))(target_data <- array(target_data,dim=c(1,length(target_data))))
    for (i in 1:dim(target_data)[1]){
    
      if(toupper(format)=="CSV")(format <- "csv")
      if(format!="csv")(format <- "nc")
      
      if (format=="nc"){

      # create netcdf
        
        # NetCDF format 3 or 4
  
        if (nc34==4){
          nc_format <- as.logical(1)
          compression = 4
        } else {
          nc_format <- as.logical(0)
          compression = NA
        }
        
      # create filename
      index <- 1
      if (!missing(station_names)){
        if (length(station_names)==length(lon1)){
          outfile <- file.path(outpath,paste(station_names[i],".nc",sep=""))
        } else {
            cat("station_names not used because length is not equal lon1.", "\n")
            outfile <- file.path(outpath,paste("selpoint_",target_lon[i],"_",target_lat[i],".nc",sep=""))
            while (file.exists(outfile)) { 
              outfile <- file.path(outpath,paste("selpoint",index,"_",target_lon[i],"_",target_lat[i],".nc",sep=""))
              index <- index+1
            }
        }
      } else {
         outfile <- file.path(outpath,paste("selpoint_",target_lon[i],"_",target_lat[i],".nc",sep=""))
         while (file.exists(outfile)) { 
           outfile <- file.path(outpath,paste("selpoint",index,"_",target_lon[i],"_",target_lat[i],".nc",sep=""))
           index <- index+1
         }
      }
      
      cat(paste("create netcdf:",sep=""), "\n")
      cat(outfile, "\n")

      data1 <- target_data[i,]
      lon <- target_lon[i]
      lat <- target_lat[i]
      data1[is.na(data1)] <- v_missing_value
      cmsaf_info <- (paste("cmsaf::selpoint.multi for variable ",var,sep=""))

      x <- ncdim_def(name="lon",units=lon_units,vals=lon)
      y <- ncdim_def(name="lat",units=lat_units,vals=lat)
      t <- ncdim_def(name="time",units=t_units,vals=time1,unlim=TRUE)
      if ("time_bnds" %in% varnames){
        nb2 <- c(0,1)
        tb <- ncdim_def(name="nb2",units=nb2_units,vals=nb2)
      }

      var1 <- ncvar_def(name=var,units=v_units,dim=list(x,y,t),missval=v_missing_value,
                      prec=var_prec,compression=compression)

      if ("time_bnds" %in% varnames){
        var2 <- ncvar_def(name="time_bnds",units="1",dim=list(tb,t),prec="double")
        vars <- list(var1,var2)
        ncnew <- nc_create(outfile,vars,force_v4=nc_format)

        ncvar_put(ncnew,var1,data1)
        ncvar_put(ncnew,var2,tbnds)

        ncatt_put(ncnew,var,"standard_name",v_standard_name,prec="text")
        ncatt_put(ncnew,var,"long_name",v_long_name,prec="text")
        ncatt_put(ncnew,var,"cmsaf_info",cmsaf_info,prec="text")

        ncatt_put(ncnew,"time","standard_name",t_standard_name,prec="text")
        ncatt_put(ncnew,"time","calendar",t_calendar,prec="text")
        ncatt_put(ncnew,"time","bounds","time_bnds",prec="text")

        ncatt_put(ncnew,"lon","standard_name",lon_standard_name,prec="text")
        ncatt_put(ncnew,"lon","long_name",lon_long_name,prec="text")
        ncatt_put(ncnew,"lon","axis",lon_axis,prec="text")

        ncatt_put(ncnew,"lat","standard_name",lat_standard_name,prec="text")
        ncatt_put(ncnew,"lat","long_name",lat_long_name,prec="text")
        ncatt_put(ncnew,"lat","axis",lat_axis,prec="text")

        ncatt_put(ncnew,0,"Info",info,prec="text")

      } else {
        vars <- list(var1)
        ncnew <- nc_create(outfile,vars,force_v4=nc_format)

        ncvar_put(ncnew,var1,data1)

        ncatt_put(ncnew,var,"standard_name",v_standard_name,prec="text")
        ncatt_put(ncnew,var,"long_name",v_long_name,prec="text")
        ncatt_put(ncnew,var,"cmsaf_info",cmsaf_info,prec="text")

        ncatt_put(ncnew,"time","standard_name",t_standard_name,prec="text")
        ncatt_put(ncnew,"time","calendar",t_calendar,prec="text")

        ncatt_put(ncnew,"lon","standard_name",lon_standard_name,prec="text")
        ncatt_put(ncnew,"lon","long_name",lon_long_name,prec="text")
        ncatt_put(ncnew,"lon","axis",lon_axis,prec="text")

        ncatt_put(ncnew,"lat","standard_name",lat_standard_name,prec="text")
        ncatt_put(ncnew,"lat","long_name",lat_long_name,prec="text")
        ncatt_put(ncnew,"lat","axis",lat_axis,prec="text")

        ncatt_put(ncnew,0,"Info",info,prec="text")

      }

      nc_close(ncnew)
  
    } # end format="nc"

    if (format=="csv"){
       # create filename
      index <- 1
      if (!missing(station_names)){
        if (length(station_names)==length(lon1)){
          outfile <- file.path(outpath,paste(station_names[i],".csv",sep=""))
        } else {
            cat("station_names not used because length is not equal lon1.", "\n")
            outfile <- file.path(outpath,paste("selpoint_",target_lon[i],"_",target_lat[i],".csv",sep=""))
            while (file.exists(outfile)) { 
              outfile <- file.path(outpath,paste("selpoint",index,"_",target_lon[i],"_",target_lat[i],".csv",sep=""))
              index <- index+1
            }
        }
      } else {
         outfile <- file.path(outpath,paste("selpoint_",target_lon[i],"_",target_lat[i],".csv",sep=""))
         while (file.exists(outfile)) { 
           outfile <- file.path(outpath,paste("selpoint",index,"_",target_lon[i],"_",target_lat[i],".csv",sep=""))
           index <- index+1
         }
      }
      
      cat(paste("create csv-file:",sep=""), "\n")
      cat(outfile, "\n")

      data1 <- target_data[i,]
      lon <- target_lon[i]
      lat <- target_lat[i]

      Data <- data1
      Time <- get_time(t_units,time1)
      Longitude <- rep(lon,length(time1))
      Latitude <- rep(lat,length(time1))
      dataframe <- data.frame(Time,Data,Longitude,Latitude)
      write.table(dataframe,file=outfile,row.names=FALSE,sep=";")    
    }

  } # end for dim(target_data)[1] 
    
  end.time <- Sys.time()
  cat("processing time: ",round(as.numeric(end.time-start.time,units="secs"),digits=2)," s", sep="", "\n")
  } # end if target_data not empty
} else {cat("There is something wrong with your input.","\n")}
}
