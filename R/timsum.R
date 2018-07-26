timsum <-
function(var,infile,outfile,nc34=3){

  start.time <- Sys.time()

# check filename

  filecheck <- checkfile(infile,outfile)

  if (filecheck[[1]]){
    infile <- filecheck[[2]]
    outfile <- filecheck[[3]]

# user define section

  limit <- 2601*2601*31	  # This value can be ajusted to avoid RAM overflow

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
   }else{
      nc_close(id)
      stop(cat(paste("Variable ",var," not found! File contains: ",varnames,sep="")),"\n")}

  if (v__FillValue == "undefined"){ 
    v__FillValue = v_missing_value}
  if (v_missing_value == "undefined"){ 
    v_missing_value = v__FillValue}

  # check data dimensions 
  
  if ((as.numeric(length(lon))*as.numeric(length(lat))*as.numeric(time_len))<limit){

    # calculate temporal mean in a sequence depending on limit

    dum_dat <- ncvar_get(id,var,collapse_degen=FALSE)
    cat("sum up", "\n")
    target <- rowMeans(dum_dat,dims=2,na.rm=T)
  } else {

   dum1 <- round((limit/length(lon))/length(lat))
   dum2 <- seq(1,time_len,dum1)
   dum3 <- array(dum1,dim=c(length(dum2)))
   cor <- dum1*length(dum2)-time_len
   dum3[length(dum2)] <- dum3[length(dum2)]-cor
  
   sum_data <- array(NA,dim=c(length(lon),length(lat),length(dum2)))

   cat("sum up sequentially",sep="","\n")
   for (i in 1:length(dum2)){
    dum_dat <- ncvar_get(id,var,start=c(1,1,dum2[i]),count=c(-1,-1,dum3[i]),collapse_degen=FALSE)
    sum_data[,,i] <- rowSums(dum_dat,dims=2,na.rm=T)
   }
    target <- rowSums(sum_data,dims=2,na.rm=T)
  }
   nc_close(id)

# create netcdf

  time_bnds <- array(NA, dim=c(2,1))
  time_bnds[1,1] <- min(time1)
  time_bnds[2,1] <- max(time1)

  cat("create netcdf", "\n")

  # NetCDF format 3 or 4
  
  if (nc34==4){
    nc_format <- as.logical(1)
    compression = 4
  } else {
    nc_format <- as.logical(0)
    compression = NA
  }

    cmsaf_info <- (paste("cmsaf::timsum for variable ",var,sep=""))
    target[is.na(target)] <- v_missing_value

    nb2 <- c(0,1)
    times <- time_bnds[1,]
    
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
    t <- ncdim_def(name="time",units=t_units,vals=times,unlim=TRUE)
    tb <- ncdim_def(name="nb2",units="1",vals=nb2)

    var1 <- ncvar_def(name=var,units=v_units,dim=list(x,y,t),missval=v_missing_value,
                      prec=var_prec,compression=compression)
    var2 <- ncvar_def(name="time_bnds",units="1",dim=list(tb,t),prec="double")
    vars <- list(var1,var2)
    ncnew <- nc_create(outfile,vars,force_v4=nc_format)

    ncvar_put(ncnew,var1,target)
    ncvar_put(ncnew,var2,time_bnds)

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

 nc_close(ncnew)

  end.time <- Sys.time()
  cat("processing time: ",round(as.numeric(end.time-start.time,units="secs"),digits=2)," s",sep="", "\n")
  } # endif filecheck
}
