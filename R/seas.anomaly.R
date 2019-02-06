seas.anomaly <-
function(var,infile,outfile,nc34=3){

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

   if (var %in% varnames){
    for (i in 1:6){
      att_dum <- ncatt_get(id,var,att_list[i])
      if (att_dum$hasatt){
	      assign(v_att_list[i],att_dum$value)}
    }

    # get details of file

	  lon <- ncvar_get(id,lon_name)
	  lat <- ncvar_get(id,lat_name)
	  time1 <- ncvar_get(id,t_name)
	  time_len <- length(time1)
  
	  # calculate field maximum 
	
	  maxval <- array(NA,dim=c(3))
	  if (time_len>=3){
	   samp <- sample(c(1:time_len),3)
	  } else {
	      samp <- 1
	    }
	  count <- 1
	  for (i in samp){
	   data1 <- ncvar_get(id,var,start=c(1,1,i),count=c(-1,-1,1))
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
  
   }else{
      nc_close(id)
      stop(cat(paste("Variable ",var," not found! File contains: ",varnames,sep="")),"\n")}

  if (v__FillValue == "undefined"){ 
    v__FillValue = v_missing_value}
  if (v_missing_value == "undefined"){ 
    v_missing_value = v__FillValue}

  nc_close(id)   

# extract time information

  date.time <- as.Date(get_time(t_units,time1))
  mon  <- as.integer(format(date.time,"%m"))
  year <- as.integer(format(date.time,"%Y"))
  yl   <- as.integer(unique(year))

  target <- array(NA,dim=c(length(lon),length(lat),1))
  tbnds <- array(NA, dim=c(2,1))

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

    cmsaf_info <- (paste("cmsaf::seas.anomaly for variable ",var,sep=""))
    target[is.na(target)] <- v_missing_value
    nb2 <- c(0,1)
    
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

    x <- ncdim_def(name="lon",units=lon_units,vals=lon)
    y <- ncdim_def(name="lat",units=lat_units,vals=lat)
    t <- ncdim_def(name="time",units=t_units,vals=0,unlim=TRUE)
    tb <- ncdim_def(name="nb2",units="1",vals=nb2)

    var1 <- ncvar_def(name=var,units=v_units,dim=list(x,y,t),missval=v_missing_value,
                      prec=var_prec,compression=compression)
    var2 <- ncvar_def(name="time_bnds",units="1",dim=list(tb,t),prec=var_prec)
    vars <- list(var1,var2)
    ncnew <- nc_create(outfile,vars,force_v4=nc_format)

    ncvar_put(ncnew,var1,target)
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
    
    if (length(global_att_list)>0){
      for (iglob in 1:length(global_att_list)){
        ncatt_put(ncnew,0,global_att_list[iglob],global_att[iglob][[1]],prec="text")
      }
    }

 # get data and calculate multi-year seasonal means

  id <- nc_open(infile)
  seas_clim  <- array(NA,dim=c(length(lon),length(lat),4))

    for (j in 1:4){
      seas_dummy <- NA
      
      if(j==1){
        win <- which(mon==1|mon==2|mon==12)
        if (length(win)>=1){seas_dummy <- win}
      }
      if(j==2){
        spr <- which(mon==3|mon==4|mon==5)
        if (length(spr)>=1){seas_dummy <- spr}
      }
      if(j==3){
        sum <- which(mon==6|mon==7|mon==8)
        if (length(sum)>=1){seas_dummy <- sum}
      }
      if(j==4){
        aut <- which(mon==9|mon==10|mon==11)
        if (length(aut)>=1){seas_dummy <- aut}
      }
      
	    dum_dat <- array(NA,dim=c(length(lon),length(lat),length(seas_dummy)))
	    
	    for (i in 1:length(seas_dummy)){
	      if (!is.na(seas_dummy[i])){
	        dum_dat[,,i] <- ncvar_get(id,var,start=c(1,1,seas_dummy[i]),count=c(-1,-1,1),collapse_degen=FALSE)
	      }
	    }
	    
	    cat("\r","calculate multi-year seasonal mean ",j," of 4",sep="")
	    seas_clim[,,j] <- rowMeans(dum_dat,dims=2,na.rm=T)
    }

  # calculate seasonal means

  count <- 1
  cat("\n")
  
  for (i in 1:length(yl)){
    for (j in 1:4){
      seas_dummy <- NA
      
      if(j==1){
        win <- which(mon==1&year==yl[i]|mon==2&year==yl[i]|mon==12&year==(yl[i]-1))
        if (length(win)>=1){seas_dummy <- win}
      }
      if(j==2){
        spr <- which(mon==3&year==yl[i]|mon==4&year==yl[i]|mon==5&year==yl[i])
        if (length(spr)>=1){seas_dummy <- spr}
      }
      if(j==3){
        sum <- which(mon==6&year==yl[i]|mon==7&year==yl[i]|mon==8&year==yl[i])
        if (length(sum)>=1){seas_dummy <- sum}
      }
      if(j==4){
        aut <- which(mon==9&year==yl[i]|mon==10&year==yl[i]|mon==11&year==yl[i])
        if (length(aut)>=1){seas_dummy <- aut}
      }
      
      if(!all(is.na(seas_dummy))){
        dum_dat <- array(NA,dim=c(length(lon),length(lat),length(seas_dummy)))
      
        for (k in 1:length(seas_dummy)){
          if (!is.na(seas_dummy[k])){
            dum_dat[,,k] <- ncvar_get(id,var,start=c(1,1,seas_dummy[k]),count=c(-1,-1,1),collapse_degen=FALSE)
          }
        }
      
        cat("\r","apply seasonal mean ",count," of ",(length(yl)*4),sep="")
	      mean_data <- rowMeans(dum_dat,dims=2,na.rm=T)
	      mean_data <- mean_data-seas_clim[,,j]
	      mean_data[is.na(mean_data)] <- v_missing_value
	      tdum <- min(time1[seas_dummy],na.rm=T)
	      tbnds[1,1] <- min(time1[seas_dummy],na.rm=T)
	      tbnds[2,1] <- max(time1[seas_dummy],na.rm=T)
	      ncvar_put(ncnew,var1,mean_data,start=c(1,1,count),count=c(-1,-1,1))
	      ncvar_put(ncnew,t,tdum,start=count,count=1)
	      ncvar_put(ncnew,var2,tbnds,start=c(1,count),count=c(-1,1))
	      count <- count+1
      }
	   }
    }
    

 nc_close(id)

 nc_close(ncnew)

  end.time <- Sys.time()
  cat("\n","processing time: ",round(as.numeric(end.time-start.time,units="secs"),digits=2)," s",sep="", "\n")
  } # endif filecheck
}
