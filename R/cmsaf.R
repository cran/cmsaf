#'cmsaf: A package for analyzing and manipulating CM SAF NetCDF formatted data.
#'
#'The 'cmsaf' functions are manipulating NetCDF input files and write the result
#'in a separate output file. The functions were designed and tested for CM SAF
#'NetCDF data, but most of the functions can be applied to other NetCDF data,
#'which use the CF convention. As interface to NetCDF data the
#'\link[ncdf4]{ncdf4-package} is used.
#'
#'@section Toolbox: \code{\link{run_toolbox}} (user-friendly shiny app)
#'
#'@section Mathematical operators: \code{\link{cmsaf.add}},
#'  \code{\link{cmsaf.addc}}, \code{\link{cmsaf.div}}, \code{\link{cmsaf.divc}},
#'  \code{\link{cmsaf.mul}}, \code{\link{cmsaf.mulc}}, \code{\link{cmsaf.sub}},
#'  \code{\link{cmsaf.subc}}, \code{\link{divdpm}}, \code{\link{muldpm}}
#'
#'@section Daily statistics: \code{\link{dayrange}}, \code{\link{ydaymean}}
#'
#'@section Monthly statistics: \code{\link{mon.anomaly}}, \code{\link{monmax}},
#'  \code{\link{monmean}}, \code{\link{monmin}}, \code{\link{monsd}},
#'  \code{\link{monsum}},
#'  \code{\link{multimonmean}}, \code{\link{multimonsum}}, \code{\link{ymonmax}},
#'  \code{\link{ymonmean}}, \code{\link{ymonmin}}, \code{\link{ymonsd}},
#'  \code{\link{ymonsum}}
#'
#'@section Seasonal statistics: \code{\link{seas.anomaly}},
#'  \code{\link{seasmean}}, \code{\link{seassum}}, \code{\link{yseasmax}},
#'  \code{\link{yseasmean}}, \code{\link{yseasmin}}, \code{\link{yseassd}}
#'
#'@section Annual statistics: \code{\link{year.anomaly}},
#'  \code{\link{yearmean}}, \code{\link{yearsum}}
#'
#'@section Temporal operators: \code{\link{timmax}},
#'  \code{\link{timmean}}, \code{\link{timmin}}, \code{\link{timpctl}},
#'  \code{\link{timsd}}, \code{\link{timsum}}, \code{\link{trend}}
#'
#'@section Spatial operators: \code{\link{fldmax}},
#'  \code{\link{fldmean}}, \code{\link{fldmin}}, \code{\link{wfldmean}}
#'
#'@section Selection and removal functions: \code{\link{extract.level}},
#'  \code{\link{extract.period}}, \code{\link{sellonlatbox}},
#'  \code{\link{selmon}}, \code{\link{selperiod}}, \code{\link{selpoint.multi}},
#'  \code{\link{selpoint}}, \code{\link{seltime}}, \code{\link{selyear}}
#'
#'@section Data manipulation: \code{\link{box_mergetime}},
#'  \code{\link{levbox_mergetime}}, \code{\link{add_grid_info}},
#'  \code{\link{remap}}
#'
#'@section Other functions:
#'  \code{\link{cmsaf.cat}}, \code{\link{get_time}}, \code{\link{ncinfo}},
#'  \code{\link{read_ncvar}}
#'
#'@docType package
#'@name cmsaf
#'
#'@importFrom ncdf4 nc_open nc_close nc_create ncdim_def ncvar_def ncvar_get
#'  ncvar_put ncvar_add ncvar_rename ncvar_change_missval ncatt_get ncatt_put
#'  nc_sync
#'
#'@author Maintainer: Steffen Kothe \email{Steffen.Kothe@dwd.de}
#'
#'  Contact: CM SAF Team \email{contact.cmsaf@dwd.de}
#'
#'@references \url{http://www.cmsaf.eu}
#'
#'  Kothe, S.; Hollmann, R.; Pfeifroth, U.; Träger-Chatterjee, C.; Trentmann, J.
#'  The CM SAF R Toolbox—A Tool for the Easy Usage of Satellite-Based Climate Data
#'  in NetCDF Format. ISPRS Int. J. Geo-Inf. 2019, 8, 109.
#'  \url{https://doi.org/10.3390/ijgi8030109}
#'
#'@keywords datagen manip package spatial ts univar
NULL
