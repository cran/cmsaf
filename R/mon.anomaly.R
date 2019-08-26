#'Determine monthly anomalies
#'
#'The function subtracts from each timestep of a time series the corresponding
#'multi-year monthly mean. To get monthly anomalies, the input file should
#'contain monthly mean values.
#'
#'@param var Name of NetCDF variable (character).
#'@param infile Filename of input NetCDF file. This may include the directory
#'  (character).
#'@param outfile Filename of output NetCDF file. This may include the directory
#'  (character).
#'@param nc34 NetCDF version of output file. If \code{nc34 = 3} the output file will be
#'  in NetCDFv3 format (numeric). Default output is NetCDFv4.
#'@param overwrite logical; should existing output file be overwritten?
#'@param verbose logical; if TRUE, progress messages are shown
#'
#'@return A NetCDF file including a time series of differences is written.
#'@export
#'
#'@family monthly statistics
#'
#' @examples
#'## Create an example NetCDF file with a similar structure as used by CM
#'## SAF. The file is created with the ncdf4 package.  Alternatively
#'## example data can be freely downloaded here: <https://wui.cmsaf.eu/>
#'
#'library(ncdf4)
#'
#'## create some (non-realistic) example data
#'
#'lon <- seq(5, 15, 0.5)
#'lat <- seq(45, 55, 0.5)
#'time <- seq(as.Date("2000-01-01"), as.Date("2010-12-31"), "month")
#'origin <- as.Date("1983-01-01 00:00:00")
#'time <- as.numeric(difftime(time, origin, units = "hour"))
#'data <- array(250:350, dim = c(21, 21, 132))
#'
#'## create example NetCDF
#'
#'x <- ncdim_def(name = "lon", units = "degrees_east", vals = lon)
#'y <- ncdim_def(name = "lat", units = "degrees_north", vals = lat)
#'t <- ncdim_def(name = "time", units = "hours since 1983-01-01 00:00:00",
#'  vals = time, unlim = TRUE)
#'var1 <- ncvar_def("SIS", "W m-2", list(x, y, t), -1, prec = "short")
#'vars <- list(var1)
#'ncnew <- nc_create("CMSAF_example_file.nc", vars)
#'ncvar_put(ncnew, var1, data)
#'ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
#'ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
#'nc_close(ncnew)
#'
#'## Determine the monthly anomalies of the example CM SAF NetCDF file and
#'## write the output to a new file.
#'mon.anomaly("SIS", "CMSAF_example_file.nc",
#'  "CMSAF_example_file_mon.anomaly.nc")
#'
#'unlink(c("CMSAF_example_file.nc", "CMSAF_example_file_mon.anomaly.nc"))
mon.anomaly <- function(var, infile, outfile, nc34 = 4, overwrite = FALSE, verbose = FALSE) {
  calc_time_start <- Sys.time()

  check_variable(var)
  check_infile(infile)
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  check_nc_version(nc34)

  ##### extract data from file #####
  file_data <- read_file(infile, var)
  file_data$variable$prec <- "float"
  months_all <- get_date_time(file_data$dimension_data$t, file_data$time_info$units)$months
  months_unique <- sort(unique(months_all))

  # Use placeholder for result so that it can be calculated later without the
  # need to have all input data in memory concurrently.
  data_placeholder <- array(
    file_data$variable$attributes$missing_value,
    dim = c(length(file_data$dimension_data$x),
            length(file_data$dimension_data$y),
            1)
  )
  if(file_data$time_info$has_time_bnds){
    time_bnds <- get_time_bounds_from_file(infile)
    vars_data <- list(result = data_placeholder, time_bounds = time_bnds)
  }else{
    vars_data <- list(result = data_placeholder)
  }


  nc_format <- get_nc_version(nc34)
  cmsaf_info <- paste0("cmsaf::mon.anomaly for variable ",
                       file_data$variable$name)

  time_data <- file_data$dimension_data$t[1]

  ##### prepare output #####
  global_att_list <- names(file_data$global_att)
  global_att_list <- global_att_list[toupper(global_att_list) %in% toupper(GLOBAL_ATT_DEFAULT)]
  global_attributes <- file_data$global_att[global_att_list]

  dims <- define_dims(file_data$grid$is_regular,
                      file_data$dimension_data$x,
                      file_data$dimension_data$y,
                      time_data,
                      NB2,
                      file_data$time_info$units,
                      with_time_bnds = file_data$time_info$has_time_bnds)

  vars <- define_vars(file_data$variable, dims, nc_format$compression, with_time_bnds = file_data$time_info$has_time_bnds)

  write_output_file(
    outfile,
    nc_format$force_v4,
    vars,
    vars_data,
    file_data$variable$name,
    file_data$grid$vars, file_data$grid$vars_data,
    cmsaf_info,
    file_data$time_info$calendar,
    file_data$variable$attributes,
    global_attributes,
    with_time_bnds = file_data$time_info$has_time_bnds
  )

  ##### calculate and write result #####
  nc_out <- nc_open(outfile, write = TRUE)
  dummy_vec <- seq_along(months_all)

  for (j in seq_along(months_unique)) {
    mon_dummy <- which(months_all == months_unique[j])
    startt <- dummy_vec[mon_dummy]
    dum_dat <- array(NA,dim=c(length(file_data$dimension_data$x),length(file_data$dimension_data$y),length(startt)))
    nc_in <- nc_open(infile)
    for (i in seq_along(startt)){
      dum_dat[,,i] <- ncvar_get(nc_in,file_data$variable$name,start=c(1,1,startt[i]),count=c(-1,-1,1),collapse_degen=FALSE)
    }

    if (verbose) message(paste0("apply monthly anomaly ", j,
                   " of ", length(months_unique)))

    mean_data <- rowMeans(dum_dat,dims=2,na.rm=TRUE)

    for (i in seq_along(startt)){
      dum_dat[,,i] <- dum_dat[,,i]-mean_data
      dum_dat[,,i][is.na(dum_dat[,,i])] <- file_data$variable$attributes$missing_value
      ncvar_put(nc_out,vars[[1]],dum_dat[,,i],start=c(1,1,startt[i]),count=c(-1,-1,1))
      ncvar_put(nc_out,dims$t,file_data$dimension_data$t[startt[i]], start=startt[i], count=1)
      if (file_data$time_info$has_time_bnds){
        ncvar_put(nc_out,vars[[2]],time_bnds[,startt[i]],start=c(1,startt[i]),count=c(-1,1))
      }
    }

    nc_close(nc_in)
  }

  nc_close(nc_out)

  calc_time_end <- Sys.time()
  if (verbose) message(get_processing_time_string(calc_time_start, calc_time_end))
}
