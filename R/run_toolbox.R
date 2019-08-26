#' Run the CMSAF toolbox.
#'
#' Run the interactive shiny-based toolbox based on the cmsaf package.
#'
#'
#' @param ... Arguments to be passed to \code{\link[shiny]{runApp}}.
#'
#' @export
#'
#' @examples \dontrun{run_toolbox(launch.browser = TRUE)}
run_toolbox <- function(...) {
  app_dir <- system.file("toolbox", package = "cmsaf")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing 'cmsaf'.",
         call. = FALSE)
  }

  dependencies <- c("colourpicker",
                    "colorspace",
                    "fields",
                    "FNN",
                    "mapproj",
                    "maps",
                    "maptools",
                    "ncdf4",
                    "R.utils",
                    "rworldxtra",
                    "shiny",
                    "shinyjs",
                    "shinythemes",
                    "sp",
                    "tcltk")
  dependencies_missing <- character(0)
  for (dependency in dependencies) {
    if (!requireNamespace(dependency, quietly = TRUE)) {
      dependencies_missing <- c(dependencies_missing, dependency)
    }
  }

  if (length(dependencies_missing) != 0) {
    if (length(dependencies_missing) == 1) {
      package_numerus <- "Package "
      pronomen_numerus <- "it"
    } else {
      package_numerus <- "Packages "
      pronomen_numerus <- "them"
    }

    stop(package_numerus, paste0(dependencies_missing, collapse = ", "),
         " needed for this function to work. Please install ", pronomen_numerus,
         " with:\n",
         "install.packages(c(",
         paste("\"", dependencies_missing, "\"", collapse = ", ", sep = ""),
         "))"
    )
  }

  shiny::runApp(app_dir, ...)
}
