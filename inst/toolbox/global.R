# Use home directory for storing config file (e.g. C:\Users\<user>\Documents
# on Windows or /home/<user> on Linux)
config_directory <- file.path(path.expand("~"), "CMSAF-Toolbox")
config_filepath <- file.path(config_directory, "config.conf")
grid_filepath <- file.path(config_directory, "myGrid.txt")

# Is a local or remote session?
isRunningLocally <- Sys.getenv("SHINY_PORT") == ""
# Test Remote
# isRunningLocally <- FALSE
remoteVolume <- c(data = "/srv/shiny-server/toolbox/data")

operatorGroups <- c("Daily statistics",
                    "Monthly statistics",
                    "Seasonal statistics",
                    "Annual statistics",
                    "Temporal operators",
                    "Spatial operators",
                    "Selection",
                    "Mathematical operators",
                    "Data manipulation",
                    "Climate Analysis")

operators <- c()

operators[["Mathematical operators"]] <- c("Add constant to data" = "cmsaf.addc",
                                           "Divide data by constant" = "cmsaf.divc",
                                           "Multiply data with constant" = "cmsaf.mulc",
                                           "Subtract constant from data" = "cmsaf.subc",
                                           "Divide by days per month" = "divdpm",
                                           "Multiply by days per month" = "muldpm",
                                           "Add values from another file" = "cmsaf.add",
                                           "Subtract values from another file" = "cmsaf.sub")

operators[["Daily statistics"]] <- c("Diurnal range" = "dayrange",
                                     "Multi-year daily means" = "ydaymean")

operators[["Monthly statistics"]] <- c("Monthly anomalies" = "mon.anomaly",
                                       "Monthly maxima" = "monmax",
                                       "Monthly means" = "monmean",
                                       "Monthly minima" = "monmin",
                                       "Monthly standard deviation" = "monsd",
                                       "Monthly sums" = "monsum",
                                       "Multi-monthly means" = "multimonmean",
                                       "Multi-monthly sums" = "multimonsum",
                                       "Multi-year monthly maxima" = "ymonmax",
                                       "Multi-year monthly means" = "ymonmean",
                                       "Multi-year monthly minima" = "ymonmin",
                                       "Multi-year monthly standard deviations" = "ymonsd",
                                       "Multi-year monthly sums" = "ymonsum")

operators[["Seasonal statistics"]] <- c("Seasonal anomalies" = "seas.anomaly",
                                        "Seasonal means" = "seasmean",
                                        "Seasonal sums" = "seassum",
                                        "Multi-year seasonal maxima" = "yseasmax",
                                        "Multi-year seasonal means" = "yseasmean",
                                        "Multi-year seasonal minima" = "yseasmin",
                                        "Multi-year seasonal standard deviations" = "yseassd")

operators[["Annual statistics"]] <- c("Annual anomalies" = "year.anomaly",
                                      "Annual means" = "yearmean",
                                      "Annual sums" = "yearsum")

operators[["Temporal operators"]] <- c("All-time maxima" = "timmax",
                                       "All-time means" = "timmean",
                                       "All-time minima" = "timmin",
                                       "All-time percentiles" = "timpctl",
                                       "All-time standard deviations" = "timsd",
                                       "All-time sums" = "timsum",
                                       "Linear trends" = "trend")

operators[["Spatial operators"]] <- c("Spatial maximum" = "fldmax",
                                      "Spatial mean" = "fldmean",
                                      "Spatial minimum" = "fldmin",
                                      "Weighted spatial mean" = "wfldmean")

operators[["Selection"]] <- c("Remove time period" = "extract.period",
                              "Select region by longitude and latitude" = "sellonlatbox",
                              "Select data at given point" = "selpoint",
                              "Select data at multiple points" = "selpoint.multi",
                              "Select list of months" = "selmon",
                              "Select time period" = "selperiod",
                              "Select list of years" = "selyear",
                              "Select list of times" = "seltime")

operators[["Data manipulation"]] <- c("Grid interpolation" = "remap")

operators[["Climate Analysis"]] <- c(
  "Absolute map" = "absolute_map",
  "Anomaly map" = "anomaly_map",
  "Climatology map" = "climatology_map",
  "Fieldmean plot" = "fieldmean_plot",
  "Fieldmean and anomaly map" = "fieldmean_and_anomaly_map"
)

operatorOptions <- c("constant",
                     "region",
                     "point",
                     "useFastTrend",
                     "dateRange",
                     "percentile",
                     "months",
                     "years",
                     "times",
                     "method",
                     "monitor_climate",
                     "file_select")

operatorOptionsDict <- c()
operatorOptionsDict[["constant"]] <- c("cmsaf.addc",
                                       "cmsaf.divc",
                                       "cmsaf.mulc",
                                       "cmsaf.subc")
operatorOptionsDict[["region"]] <- c("sellonlatbox",
                                     "absolute_map",
                                     "anomaly_map",
                                     "climatology_map",
                                     "fieldmean_plot",
                                     "fieldmean_and_anomaly_map")
operatorOptionsDict[["point"]] <- c("selpoint", "selpoint.multi")
operatorOptionsDict[["useFastTrend"]] <- c("trend")
operatorOptionsDict[["dateRange"]] <- c("selperiod",
                                     "extract.period",
                                     "absolute_map",
                                     "anomaly_map",
                                     "climatology_map",
                                     "fieldmean_plot",
                                     "fieldmean_and_anomaly_map")
operatorOptionsDict[["percentile"]] <- c("timpctl")
operatorOptionsDict[["months"]] <- c("selmon", "multimonmean", "multimonsum")
operatorOptionsDict[["years"]] <- c("selyear")
operatorOptionsDict[["times"]] <- c("seltime")
operatorOptionsDict[["method"]] <- c("remap")
operatorOptionsDict[["monitor_climate"]] <- c("absolute_map",
                                              "anomaly_map",
                                              "climatology_map",
                                              "fieldmean_plot",
                                              "fieldmean_and_anomaly_map")
operatorOptionsDict[["file_select"]] <- c("cmsaf.add", "cmsaf.sub")


# default plot settings
textsize    <- 1.2
linesize    <- 1.5
bordercolor <- "gray20"
# imagewidth  <- -1         # if -1 image dimensions are taken from data
# imageheight <- -1
na.color    <- "gray80"
image_def <- 800         # default image size
ihsf      <- 0.1         # default image heigth scale factor
grid_col  <- "cornsilk2" # default color of grid lines
plot_grid <- TRUE        # plot grid lines (TRUE = yes, FALSE = no)

# Load countriesHigh data.
countriesHigh <- numeric(0)
data(countriesHigh, package = "rworldxtra")

# Valid countries  # TODO duplicated code in cmsafvis/data-raw/generate_internal_data.R
# The package needs only three specific columns from the original data.
codes <- countrycode::codelist[, c("iso3c", "country.name.en", "country.name.de")]
# Apparently, in R 4.0.0 this returns a tibble which leads to an error because
# tibble indexing works differently. See #873 and #894.
codes <- as.data.frame(codes)

# Clear out rows where iso3c is NA. Since we always use iso3c as 'origin',
# we don't need these rows.
codes <- codes[!is.na(codes["iso3c"]), ]

# Only allow countries which are represented in countriesHigh
ids <- codes[, "iso3c"] %in% countriesHigh$ISO3.1
codes <- codes[ids, ]

# Add data for special cases.
codes <- rbind(
  codes,
  c("EUR", "Europe", "Europa"),
  c("AFR", "Africa", "Afrika"),
  c("TOT", "Totaldisk", "Gesamtausschnitt"),
  c("S_A", "Selected Area", "Gewähltes Gebiet")
)

source("ColorspacePrivates.R")
