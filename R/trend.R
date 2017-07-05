trend <-
function(var,infile,outfile,nc34=3){

  start.time <- Sys.time()

# simple function for linear regression without intercept
# adapted from Stackoverflow forum

  simplelm <- function (x, y) {
    ## number of data
    n <- length(x)
    ## centring
    y0 <- sum(y) / length(y); yc <- y - y0
    x0 <- sum(x) / length(x); xc <- x - x0
    ## fitting an intercept-free model: yc ~ xc + 0
    xty <- c(crossprod(xc, yc))
    xtx <- c(crossprod(xc))
    slope <- xty / xtx
    rc <- yc - xc * slope
    ## Pearson estimate of residual standard error
    sigma2 <- c(crossprod(rc)) / (n - 2)
    ## standard error for slope
    slope_se <- sqrt(sigma2 / xtx)
    ## confidence interval
    lo_conf <- slope - (1.959964 * slope_se)
    up_conf <- slope + (1.959964 * slope_se)
    ## return estimation summary for slope and confidence interval
    c(slope,lo_conf,up_conf)
  }
  

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

   s_name = "sig"
   s_standard_name = "significance"
   s_long_name = "significance based on 95% confidence interval"
   s_units = "1"
   s__FillValue = "undefined"
   s_missing_value = "undefined"
   s_info = "1 = positive significant, 0 = not significant, -1 = negative significant"

   info = "Created with the CM SAF R toolbox." 
   var_prec="float"

   att_list <- c("standard_name","long_name","units","_FillValue","missing_value","calendar")
   v_att_list <- c("v_standard_name","v_long_name","v_units","v__FillValue","v_missing_value","v_calendar")
  
# get file information

  cat("get file information", "\n")

  id <- nc_open(infile)

  # get information about dimensions

  dimnames <- names(id$dim)
  dimnames <- dimnames[!dimnames %in% "nb2"] # this can cause trouble

     # check standard_names of dimensions
    for (i in 1:length(dimnames)){
	    sn <- ncatt_get(id,dimnames[i],"standard_name")
	    ln <- ncatt_get(id,dimnames[i],"long_name")
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
      
    target <- array(NA,dim=c(length(lon),length(lat),1))
    target2 <- array(NA,dim=c(length(lon),length(lat),1))
    target_p <- array(NA,dim=c(length(lon),length(lat),1))
    x <- c(1:time_len)
    cat("fit with linear model...",sep="", "\n")

    for (i in 1:length(lon)){
	    prog <- round((100/length(lon))*i)
	    cat("\r","progress: ",prog,"%",sep="")
	    for (j in 1:length(lat)){
	      dum_dat <- ncvar_get(id,var,start=c(i,j,1),count=c(1,1,-1))
	      if (time_len-(sum(is.na(dum_dat)))>=2){
	        dummy <- which(is.finite(dum_dat))
	        fit <- simplelm(x[dummy],dum_dat[dummy])
	        val <- fit[1]*time_len
	        val2 <- fit[1]
	        sig <- 0
	        if (fit[2]*fit[3]<0)(sig <- 0)
	        if (fit[2]<0&fit[3]<0)(sig <- -1)
	        if (fit[2]>0&fit[3]>0)(sig <- 1)
	      } else {
	        val <- NA
	        val2 <- NA
	        sig <- NA
	      }
	    target[i,j,1] <- val
	    target2[i,j,1] <- val2
	    target_p[i,j,1] <- sig
	    }
    }

   nc_close(id)

 # create netcdf

  cat("\n","create netcdf",sep="","\n")

  # NetCDF format 3 or 4
  
  if (nc34==4){
    nc_format <- as.logical(1)
    compression = 4
  } else {
    nc_format <- as.logical(0)
    compression = NA
  }

    target[is.na(target)] <- v_missing_value
    target2[is.na(target2)] <- v_missing_value
    target_p[is.na(target)] <- v_missing_value

    time_bnds <- array(NA, dim=c(2,1))
    time_bnds[1,1] <- min(time1)
    time_bnds[2,1] <- max(time1)

    nb2 <- c(0,1)
    times <- time_bnds[1,]
    
    long_name_1 <- paste("linear trend in ",var," multiplied by length of time series",sep="")
    long_name_2 <- paste("linear trend in ",var,sep="")
    
    var_1 <- paste(var,"_trend1",sep="")
    var_2 <- paste(var,"_trend2",sep="")
    
    x <- ncdim_def(name="lon",units=lon_units,vals=lon)
    y <- ncdim_def(name="lat",units=lat_units,vals=lat)
    t <- ncdim_def(name="time",units=t_units,vals=times,unlim=TRUE)
    tb <- ncdim_def(name="nb2",units="1",vals=nb2)

    var1 <- ncvar_def(name=var_1,units=v_units,dim=list(x,y,t),missval=v_missing_value,
                      prec=var_prec,compression=compression)
    var2 <- ncvar_def(name="time_bnds",units="1",dim=list(tb,t),prec="double")
    var3 <- ncvar_def(name=s_name,units=s_units,dim=list(x,y,t),prec="double",
		      missval=v_missing_value,compression=compression)
    var4 <- ncvar_def(name=var_2,units=v_units,dim=list(x,y,t),missval=v_missing_value,
                      prec=var_prec,compression=compression)

      vars <- list(var1,var2,var3,var4)
      ncnew <- nc_create(outfile,vars,force_v4=nc_format)

      ncvar_put(ncnew,var1,target)
      ncvar_put(ncnew,var2,time_bnds)
      ncvar_put(ncnew,var3,target_p)
      ncvar_put(ncnew,var4,target2)

      ncatt_put(ncnew,var_1,"standard_name",v_standard_name,prec="text")
      ncatt_put(ncnew,var_1,"long_name",long_name_1,prec="text")
      
      ncatt_put(ncnew,var_2,"standard_name",v_standard_name,prec="text")
      ncatt_put(ncnew,var_2,"long_name",long_name_2,prec="text")

      ncatt_put(ncnew,s_name,"standard_name",s_standard_name,prec="text")
      ncatt_put(ncnew,s_name,"long_name",s_long_name,prec="text")
      ncatt_put(ncnew,s_name,"description",s_info,prec="text")

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
     
   nc_close(ncnew)

  end.time <- Sys.time()
  cat("\n","processing time: ",round(as.numeric(end.time-start.time,units="secs"),digits=2)," s",sep="", "\n")
  } # endif filecheck
}
