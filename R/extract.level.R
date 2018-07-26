extract.level <-
function(var,infile,outfile,level=1,nc34=3){

  start.time <- Sys.time()

# check filename

  filecheck <- checkfile(infile,outfile)

  if (filecheck[[1]]){
    infile <- filecheck[[2]]
    outfile <- filecheck[[3]]  

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

  # get information about dimensions and attributes
  
  dimnames   <- names(id$dim)
  global_att <- ncatt_get(id,0)

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

    # get data of selected level
	  lon <- ncvar_get(id,lon_name)
	  lat <- ncvar_get(id,lat_name)
	  time1 <- ncvar_get(id,t_name)
	  time_len <- length(time1)
	  if ("time_bnds" %in% varnames){
	    tbnds1 <- ncvar_get(id,"time_bnds",collapse_degen=FALSE)
	  }
	
	  # check level
	  if (length(dimnames==4)){
	    start <- c(1,1,1,1)
	    count <- c(-1,-1,-1,-1)
	    # identify level dimension
	    dummy <- match(dimnames,c(t_name,lon_name,lat_name))
	    leveldim <- which(is.na(dummy))
	    levellen <- id$dim[[leveldim]]$len
	  
	    if (level!="all"){
	      if (level>levellen){
	        stop(cat(paste("Dimension ",id$dim[[leveldim]]$name," has length: ",levellen,sep="")),"\n")
	      }
	      loop <- 1
	      start[3] <- level
	      count[3] <- 1
	      data <- ncvar_get(id,var,start=start,count=count)
	    } else {
	      loop <- levellen
	      data <- ncvar_get(id,var,start=start,count=count)
	    }
	  }

  } else {
      nc_close(id)
      stop(cat(paste("Variable ",var," not found! File contains: ",varnames,sep="")),"\n")}

  if (v__FillValue == "undefined"){ 
    v__FillValue = v_missing_value}
  if (v_missing_value == "undefined"){ 
    v_missing_value = v__FillValue}

  nc_close(id)

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
  
    cmsaf_info <- (paste("cmsaf::extract.level for variable ",var," and level ",level,sep=""))
  
    # prepare global attributes
    global_att_default <- c("institution","title","summary","id","creator_name",
                            "creator_email","creator_url","creator_type","publisher_name",
                            "publisher_email","publisher_url","publisher_type",
                            "references","keywords_vocabulary","keywords","project",
                            "standard_name_vocabulary","geospatial_lat_units",
                            "geospatial_lon_units","geospatial_lat_resolution",
                            "geospatial_lon_resolution","platform_vocabulary","platform",
                            "instrument_vocabulary","instrument","date_created","product_version",
                            "producer","version","dataset_version","source")
    
    global_att_list <- names(global_att)
    
    global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(global_att_default)]
    global_att <- global_att[global_att_list]
    
  for (i in 1:loop){
    
    if (level!="all"){
      outfile1 <- outfile
      data1 <- data
    } else {
      outfile1 <- paste(strsplit(outfile,split=".nc"),"_level",i,".nc",sep="")
      z <- which(dim(data)==levellen)
      if (z==3&length(dim(data))==3){data1 <- data[,,i]}
      if (z==3&length(dim(data))==4){data1 <- data[,,i,]}
    }

    if (length(time1)==1){
      dummy <- array(NA,dim=c(dim(data1)[1],dim(data1)[2],1))
      dummy[,,1] <- data1
      data1 <- dummy
    }

    data1[is.na(data1)] <- v_missing_value
    nb2 <- c(0,1)

    x <- ncdim_def(name="lon",units=lon_units,vals=lon)
    y <- ncdim_def(name="lat",units=lat_units,vals=lat)
    t <- ncdim_def(name="time",units=t_units,vals=time1,unlim=TRUE)
    if ("time_bnds" %in% varnames){
      tb <- ncdim_def(name="nb2",units=nb2_units,vals=nb2)
    }

    var1 <- ncvar_def(name=var,units=v_units,dim=list(x,y,t),missval=v_missing_value,
                      prec=var_prec,compression=compression)

    if ("time_bnds" %in% varnames){
      var2 <- ncvar_def(name="time_bnds",units="1",dim=list(tb,t),prec="double")
      vars <- list(var1,var2)
      ncnew <- nc_create(outfile,vars,force_v4=nc_format)

      ncvar_put(ncnew,var1,data1)
      ncvar_put(ncnew,var2,tbnds1)

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
      
      if (length(global_att_list)>0){
        for (iglob in 1:length(global_att_list)){
          ncatt_put(ncnew,0,global_att_list[iglob],global_att[iglob][[1]],prec="text")
        }
      }

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
      
      if (length(global_att_list)>0){
        for (iglob in 1:length(global_att_list)){
          ncatt_put(ncnew,0,global_att_list[iglob],global_att[iglob][[1]],prec="text")
        }
      }
    }

    nc_close(ncnew)
    
  } # end for loop

  end.time <- Sys.time()
  cat("processing time: ",round(as.numeric(end.time-start.time,units="secs"),digits=2)," s", sep="","\n")
  } # endif filecheck
}
