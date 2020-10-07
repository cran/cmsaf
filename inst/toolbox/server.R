# Function to compute first of month
firstOfMonth <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

# Get tarlist sorted by name.
getTarList <- function(path_to_tar, tar_flag = 1, includeClaasAux = FALSE) {

  ordname <- basename(path_to_tar)
  ordpath <- dirname(path_to_tar)

  if (identical(path_to_tar, character(0))) {
    stop(paste0("No infile!", var))
  }

  flist <- list.files(ordpath, substr(ordname, 1, 8), full.names = TRUE)

  tarlist <- NULL
  for (file in flist) {
    if (tar_flag == 1) {
      tarlist <- append(tarlist, utils::untar(file, list = TRUE, tar = "internal"))
    } else if (tar_flag == 2) {
      tarlist <- append(tarlist, utils::untar(file, list = TRUE, tar = Sys.getenv("TAR")))
    } else if (tar_flag == 3) {
      tarlist <- append(tarlist, utils::untar(file, list = TRUE, tar = "C:/Rtools/bin/tar.exe"))
    } else {
      tarlist <- append(tarlist, utils::untar(file, list = TRUE))
    }
  }
  # Exlude claas aux file if not wanted.
  if (!includeClaasAux) {
    tarlist <- tarlist[tarlist != "CM_SAF_CLAAS2_L2_AUX.nc"]
  }

  tarlist <- sort(tarlist)

  return(tarlist)
}

# Get CLAAS AUX FLAG
getClaasAuxFlag <- function(tarlist) {
  return("CM_SAF_CLAAS2_L2_AUX.nc" %in% tarlist)
}

# A function to extract ALL dates from a tar file.
extractAllDates <- function(path_to_tar, tar_flag) {
  # List the to be untared  file.
  tarlist <- getTarList(path_to_tar, tar_flag = tar_flag)

  # Timestep HAS TO BE RELEVANT FOR SOMETHING! FIND THAT OUT!!
  timestep <- substr(tarlist[1], 4, 4)

  allDates <- as.character(substr(tarlist, 6, 13))
  return(list(allDates = as.Date(allDates, "%Y%m%d"), timestep = timestep))
}

# A function to extract start and end dates.
extractDateRange <- function(path_to_tar, tar_flag) {

  tmp <- extractAllDates(path_to_tar, tar_flag = tar_flag)

  # Select earliest date from the files.
  date_from <- min(tmp$allDates)
  date_to   <- max(tmp$allDates)

  return(list(date_from = date_from, date_to = date_to, timestep = tmp$timestep))
}

# Function for OS-Independently choosing a directory
choose_directory <- function(caption = "Select data directory") {
  if (exists("utils::choose.dir") || exists("choose.dir")) {
    utils::choose.dir(caption = caption)
  } else {
    tcltk::tk_choose.dir(caption = caption)
  }
}

function(input, output, session) {
  #### Preparation and session set up ####
  # TODO: Setting the maximum request size. WARNING: NOT SURE WHAT'S A GOOD VALUE FOR THIS
  # FOR NOW SETTING TO 2.5 GB, as this exceeds the largest test data.
  options(shiny.maxRequestSize = 2500 * 1024^2)

  # Check if is running locally or on remote server
  # Variable can be found in global.R
  if (isRunningLocally) {
    shinyjs::show("tarFileLocal")
    shinyjs::show("ncFileLocal_analyze")
    shinyjs::show("ncFileLocal_visualize")
    shinyjs::show("shapefileLocal")

    # Let user be able to modify the output directory.
    shinyjs::show("modify_userDir")
  } else {
    shinyjs::show("tarFileRemote")
    shinyjs::show("ncFileRemote_analyze")
    shinyjs::show("ncFileRemote_visualize")
    shinyjs::show("shapefileRemote")

    # Let user be able to download the output directory.
    shinyjs::show("downloader")
  }

  dir.create(config_directory, showWarnings = FALSE)

  if (file.exists(config_filepath)) {
    # Does user dir exist in config file?
    config_lines <- try(readLines(config_filepath))
  } else {
    config_lines <- ""
  }

  # Save user dir as a global variable.
  userDir <<- ""
  outputDir <<- ""
  action_userDir_change <- reactiveVal(0)

  # 'Unique' session name
  sessionName <- paste0(session$user, format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"))
  videoDir <- reactiveVal(file.path(getwd(), "www", "video"))
  observe({
    if (dir.exists(videoDir())) {
      unlink(videoDir(), recursive = TRUE, force = TRUE)
    }
  })


  #### Reactive values ####
  # Reactive value for tar-file-path. (update action if file stays the same on new upload)
  tar_path <- reactiveVal()
  tar_path_action <- reactiveVal(0)

  # Reactive value set after untaring files
  untarVals <- reactiveVal()

  # Reactive value for date range
  dateRange_prep <- reactiveVal()

  # Reactive values for biggest possible spatial range
  spatialRange <- reactiveValues()

  # Reactive value for timestep
  timestep <- reactiveVal()

  #Reactive value for maximum level
  max_level <- reactiveVal()

  # Reactive value for dimensions
  dimensions <- reactiveVal()

  # Reactive value for a path to the aux file.
  auxFilePath <- reactiveVal()
  globalAuxFilePath <- reactiveVal()

  # Reactive value for using level or not.
  usingLevel <- reactiveVal(FALSE)

  # Reactive value for output file path (nc file)
  outputFilepath <- reactiveVal()

  # Reactive value for the nc file to be used in analyze. (update action if file stays the same on new upload)
  nc_path_analyze <- reactiveVal()
  nc_path_analyze_action <- reactiveVal(0)

  # Reactive value for which variable is used
  var_used <- reactiveVal()

  # Reactive value for current Operator Option
  currentOperatorOption <- reactiveVal()
  currentOperatorValue <- reactiveVal()

  chosen_latPoints <- reactiveVal(c())
  chosen_lonPoints <- reactiveVal(c())

  # Reactive value for chosen operators + a simulated reactive value.
  operatorDataFrame <<- data.frame()
  operatorDataFrameAction <- reactiveVal(0)

  # Repeat message on file loss (remote case only)
  repeatWarning <- reactiveVal(TRUE)

  # Second nc file for operators
  second_infile <- reactiveVal()

  # Reactive value for the nc file to be used in analyze. (update action if file stays the same on new upload)
  nc_path_visualize <- reactiveVal()
  actionVisualize <- reactiveVal(0)

  # Image path for monitor climate
  image_path_visualize <- reactiveVal()
  actionVisualizeMonitorClimate <- reactiveVal(0)

  # Shape file path
  shapeFile_path <- reactiveVal()
  shapeFile_path_action <- reactiveVal(0)

  # Storing the polygons.
  region_data <- reactiveVal()

  # Instat file path
  instat_path <- reactiveVal()
  instat_path_action <- reactiveVal(0)

  # Reactive Value for all visualize variables
  visualizeVariables <- reactiveVal()
  visualizeDataTimestep <- reactiveVal()
  visualizeDataMin <- reactiveVal()
  visualizeDataMax <- reactiveVal()
  reversedDimensions <- reactiveValues(transpose = FALSE, latReverse = FALSE, lonReverse = FALSE)

  # Variable to be visualized chosen in modal
  variable_visualize_modal <- reactiveVal()
  action_visualize_post_modal <- reactiveVal(0)

  # Reactive values for coordinate bounds in visualize
  lon_bounds <- reactiveVal()
  lat_bounds <- reactiveVal()

  # Longitute, latitute location vectors (of own locations added in options)
  lon_loc_vec  <- reactiveVal()
  lat_loc_vec  <- reactiveVal()
  name_loc_vec <- reactiveVal()

  # Png file location
  png_path <- reactiveVal()

  # Reactive values for imagewidth and imageheight
  imageheight <- reactiveVal(round(image_def * (2 / 3)))
  imagewidth  <- reactiveVal(image_def)
  lat_lon_trigger <- reactiveVal(0)

  # Reactive value to determine whether this was the first plot
  readyToPlot <- reactiveVal(FALSE)

  # Set these values to one as soon as valid.
  userDirValid <- reactiveVal(0)
  resolutionValid <- reactiveVal(0)

  # isRunningLocally can be found in global.R
  # Find/Create user directory.
  if (isRunningLocally) {
    # Case of local host!

    # Read lines of file.
    for (line in config_lines) {
      if (startsWith(line, "USRWDIR=")) {
        if (nchar(line) > 8) {
          userDir <<- substring(line, 9)
          userDir <<- file.path(userDir, "output")
          outputDir <<- userDir
          # Create the output directory.
          if (!dir.exists(userDir)) {
            dir.create(userDir)
          }
          setwd(userDir)
        }
      }
    }

    # Set flags and counts for girdInfo and userDir
    gridSupportFile <- file.path(config_directory, "myGrid.txt")
    noGridInfoFlag <<- !file.exists(gridSupportFile)
    noUserDirFlag <<- (nchar(userDir) == 0 || !dir.exists(userDir))

    checkSum <<- 0
    if (noUserDirFlag || noGridInfoFlag) {
      # Check sum = 1 unless both files are missing
      checkSum <<- 1
      if (noUserDirFlag && noGridInfoFlag) {
        checkSum <<- 2

        showModal(modalDialog(
          h4("Please select a directory where created output will be saved."),
          br(),
          actionButton("userDirButton", "Choose a user directory."),
          verbatimTextOutput("selectedUserDirectory"),
          hr(),
          h4("Please specify a grid resolution."),
          h5("The chosen value must be between 0.01 and 89.99.
              Depending on your browser a comma or a point will be accepted as decimal seperator."),
          numericInput("gridResolution", "Resolution", value = 0.05, step = 0.01, min = 0.01, max = 89.99),
          title = "We need your help.",
          size = "l",
          footer = shinyjs::disabled(actionButton("Submit", "Submit"))
        ))
      } else if (noUserDirFlag) {
        showModal(modalDialog(
          h4("Please select a directory where we can save your created output."),
          br(),
          actionButton("userDirButton", "Choose a user directory."),
          verbatimTextOutput("selectedUserDirectory"),
          title = "We need your help.",
          size = "l",
          footer = shinyjs::disabled(actionButton("Submit", "Submit"))
        ))
      } else {
        showModal(modalDialog(
          h4("Please specify a grid resolution."),
          h5("The chosen value must be between 0.01 and 89.99.
              Depending on your browser a comma or a point will be accepted as decimal seperator."),
          numericInput("gridResolution", "Resolution", value = 0.05, step = 0.01, min = 0.01, max = 89.99),
          title = "We need your help.",
          size = "l",
          footer = shinyjs::disabled(actionButton("Submit", "Submit"))
        ))
      }
    }
  } else {
    # Case of remote host!
	dataDir <- file.path("output")
    dataDir <- file.path(dirname(dataDir), "output")

    # Remember the user directory and tear it down in destructor.
    if (!dir.exists("output")) {
      warning("Creating user directory at '", getwd(), "/output'.")
      dir.create("output")
    }
    userDir   <<- file.path("output", sessionName)
    outputDir <<- userDir

    dirFlag <- FALSE
    # If by any chance this directroy has existed give warning message.
    if (dir.exists(userDir)) {
      dirFlag <- TRUE
      warning(paste0("User directory: ", userDir, " already exists. APP WILL STOP!"))
    } else {
      dir.create(userDir)
    }
  }

  # Observing when user submits initial values for config and/or myGrid.txt file.
  observeEvent(input$Submit, {
    if (noUserDirFlag) {
      # Overwrite config file (config filepath is found in global.R)
      if (file.exists(config_filepath)) {
        file.remove(config_filepath)
      }
      file.create(config_filepath)

      # Save  for next session.
      writeLines(paste0("USRWDIR=", userDir), config_filepath)

      userDir   <<- file.path(userDir, "output")
      outputDir <<- userDir
      action_userDir_change(action_userDir_change() + 1)

      if (!dir.exists(userDir)) {
        dir.create(userDir)
      }
    }

    if (noGridInfoFlag) {
      file.create(gridSupportFile)

      gridXsize <- 360 / input$gridResolution + 1
      gridYsize <- 180 / input$gridResolution + 1

      gridLines <- c("gridtype = lonlat",
                     paste0("xsize = ", gridXsize),
                     paste0("ysize = ", gridYsize),
                     "xfirst = -180",
                     paste0("xinc = ", input$gridResolution),
                     "yfirst = -90",
                     paste0("yinc = ", input$gridResolution))

      writeLines(gridLines, gridSupportFile)
    }

    removeModal()
  })

  # Observe when user gives output directory.
  observeEvent(input$userDirButton, {
    userDirValid(0)
    shinyjs::disable("userDirButton")
    try(userDir <<- choose_directory())
    if (dir.exists(userDir)) {
      output$selectedUserDirectory <- renderText({
        userDir
      })
      userDirValid(1)
      shinyjs::show("selectedUserDirectory")
      setwd(userDir)
    } else {
      shinyjs::hide("selectedUserDirectory")
    }
    shinyjs::enable("userDirButton")
  })

  # Observe when user gives grid resolution.
  observeEvent(input$gridResolution, {
    resolutionValid(0)
    req(input$gridResolution)
    if (input$gridResolution >= 0.01 && input$gridResolution <= 89.99) {
      resolutionValid(1)
    }
  })

  # Allow submitting the initial modal page, as soon as check sum fullfilled, i.e. all required inputs are valid.
  observe({
    req(isRunningLocally)
    if (resolutionValid() + userDirValid() == checkSum) {
      shinyjs::enable("Submit")
    } else {
      shinyjs::disable("Submit")
    }
  })

  observeEvent(input$modify_userDir, {
    showModal(modalDialog(
      h4("Your current user directory is located at:"),
      tags$strong(dirname(userDir)),
      br(),
      actionButton("userDirButton", "Change user directory."),
      title = "User directory information.",
      size = "l"
    ))
  })

  observe({
    # Trigger every time this changes
    c(action_userDir_change())
    output$prepareString <- renderUI({
      # Can be found in global.R
      if (isRunningLocally) {
        # Local host prepare string dependent on userDir
        tags$div(h2("Prepare"),
                 tags$p("Please select a TAR file", tags$strong("(.tar)"), " to start the preparation."),
                 tags$p("of your data. This is the first step after you downloaded your ordered tar-file(s)."),
                 br(),
                 tags$p("This application will help you to extract, unzip and merge the data."),
                 tags$p("In addition, you can select a time range and region from your data."),
                 br(),
                 tags$p("Finally, a NetCDF file will be created for you. You can find it in the output directory"),
                 tags$p("located at ", tags$strong(dirname(userDir))),
                 br(),
                 tags$p("The app guides through all steps."))
      } else {
        tags$div(h2("Prepare"),
                 tags$p("Please select a TAR file", tags$strong("(.tar)"), " to start the preparation."),
                 tags$p("of your data. This is the first step after you downloaded your ordered tar-file(s)."),
                 br(),
                 tags$p("This application will help you to extract, unzip and merge the data."),
                 tags$p("In addition, you can select a time range and region from your data."),
                 br(),
                 tags$p("Finally, a NetCDF file will be created for you."),
                 tags$p(tags$strong("Make sure to download your session files before closing this application.")),
                 br(),
                 tags$p("The app guides through all steps."))
      }
    })
  })

  # Going to home screen.
  observeEvent(input$homeimg, {
    shinyjs::hide("panel_content")
    shinyjs::show("panel_home", anim = TRUE, animType = "fade")
  })

  observeEvent(c(input$prepare, input$analyze, input$visualize), {
    shinyjs::hide("panel_home")
    shinyjs::show("panel_content", anim = TRUE, animType = "fade")
  }, ignoreInit = TRUE)

  #### Observing Uploading Events ####
  #### Updating paths ####
  # Modal for wrong file format called with type argument (.tar, .nc, .shp, .RData).
  wrong_file_modal <- function(type) {
    showModal(modalDialog(
      h4(paste("Wrong file format. Please choose",
               type,
               "file to continue.")),
      title = "Warning!",
      size = "m"
    ))
  }

  # Updating the path to the tar file. (local)
  observeEvent(input$tarFileLocal, {
    shinyjs::disable("tarFileLocal")
    res <- try(tar_path(file.choose(new = TRUE)))
    if (class(res) != "try-error") {
      #for (path in tar_path()) {
      if (!endsWith(tar_path(), ".tar")) {
        isolate(tar_path(""))
        wrong_file_modal(".tar")
      } else {
        tar_path_action(tar_path_action() + 1)
      }
      #}
    }
    shinyjs::enable("tarFileLocal")
  }, ignoreInit = TRUE)

  # Handling remote tar selection.
  shinyFiles::shinyFileChoose(input, 'tarFileRemote', session = session, roots = remoteVolume, filetypes=c('tar'))
    
  observeEvent(input$tarFileRemote, {
    pth <- shinyFiles::parseFilePaths(remoteVolume,input$tarFileRemote)
    req(nrow(pth) > 0)
    req(file.exists(pth$datapath))
    req(endsWith(pth$datapath, ".tar"))
    tar_path( pth$datapath )
    tar_path_action(tar_path_action() + 1)
  })

  # Updating the path to the nc file analyze. (local)
  observeEvent(input$ncFileLocal_analyze, {
    shinyjs::disable("ncFileLocal_analyze")
    shinyjs::disable("useOutputFile_analyze")
    res <- try(nc_path_analyze(file.choose(new = TRUE)))
    if (class(res) != "try-error") {
      if (!endsWith(nc_path_analyze(), ".nc")) {
        isolate(nc_path_analyze(""))
        wrong_file_modal(".nc")
      } else {
        nc_path_analyze_action(nc_path_analyze_action() + 1)
      }
    }
    shinyjs::enable("ncFileLocal_analyze")
    shinyjs::enable("useOutputFile_analyze")
  }, ignoreInit = TRUE)

  # Use shiny files to handle remote uploading from user directory.
  volumes_output = c("Output" = outputDir, remoteVolume)

  shinyFiles::shinyFileChoose(input, 'ncFileRemote_analyze', session = session, roots = volumes_output, filetypes=c('nc'))

  observeEvent(input$ncFileRemote_analyze, {
    pth <- shinyFiles::parseFilePaths(volumes_output,input$ncFileRemote_analyze)
    req(nrow(pth) > 0)
    req(file.exists(pth$datapath))
    if (!endsWith(pth$datapath, ".nc")) {
      isolate(nc_path_analyze(""))
      wrong_file_modal(".nc")
    } else {
      nc_path_analyze( pth$datapath )
      nc_path_analyze_action(nc_path_analyze_action() + 1)
    }
  })

  # If user chooses to take generated nc file update nc_path_analyze. (output file)
  observeEvent(input$useOutputFile_analyze, {
    nc_path_analyze(outputFilepath())
    if (!endsWith(nc_path_analyze(), ".nc")) {
      isolate(nc_path_analyze(""))
      wrong_file_modal(".nc")
    } else {
      nc_path_analyze_action(nc_path_analyze_action() + 1)
    }
  }, ignoreInit = TRUE)

  # Updating the path to the nc file visualize. (local)
  observeEvent(input$ncFileLocal_visualize, {
    shinyjs::disable("ncFileLocal_visualize")
    shinyjs::disable("useOutputFile_visualize")
    res <- try(nc_path_visualize(file.choose(new = TRUE)))
    if (class(res) != "try-error") {
      if (!endsWith(nc_path_visualize(), ".nc")) {
        isolate(nc_path_visualize(""))
        wrong_file_modal(".nc")
      } else {
        actionVisualize(actionVisualize() + 1)
      }
    }
    shinyjs::enable("ncFileLocal_visualize")
    shinyjs::enable("useOutputFile_visualize")
  }, ignoreInit = TRUE)

  # Observing changes in selected nc file visualize. (remote)
  shinyFiles::shinyFileChoose(input, 'ncFileRemote_visualize', session = session, roots = volumes_output, filetypes=c('nc'))

  observeEvent(input$ncFileRemote_visualize, {
    pth <- shinyFiles::parseFilePaths(volumes_output,input$ncFileRemote_visualize)
    req(nrow(pth) > 0)
    req(file.exists(pth$datapath))
    if (!endsWith(pth$datapath, ".nc")) {
      isolate(nc_path_visualize(""))
      wrong_file_modal(".nc")
    } else {
      nc_path_visualize( pth$datapath )
      actionVisualize(actionVisualize() + 1)
    }
  })

  # If user chooses to take generated nc file update nc_path_visualize. (output file)
  observeEvent(input$useOutputFile_visualize, {
    nc_path_visualize(outputFilepath())
    if (!endsWith(nc_path_visualize(), ".nc")) {
      isolate(nc_path_visualize(""))
      wrong_file_modal(".nc")
    } else {
      actionVisualize(actionVisualize() + 1)
    }
  }, ignoreInit = TRUE)

  # Updating path to shape file. (local)
  observeEvent(input$shapefileLocal, {
    shinyjs::disable("shapefileLocal")
    res <- try(shapeFile_path(file.choose(new = TRUE)))
    if (class(res) != "try-error") {
      if (!endsWith(shapeFile_path(), ".shp")) {
        isolate(shapeFile_path(""))
        wrong_file_modal(".shp")
      } else {
        shapeFile_path_action(shapeFile_path_action() + 1)
      }
    }
    shinyjs::enable("shapefileLocal")
  }, ignoreInit = TRUE)

  # Observing changes in selected shape file. (remote)
  shinyFiles::shinyFileChoose(input, 'shapefileRemote', session = session, roots = remoteVolume, filetypes=c('shp'))

  observeEvent(input$shapefileRemote, {
    pth <- shinyFiles::parseFilePaths(remoteVolume,input$shapefileRemote)
    req(nrow(pth) > 0)
    req(file.exists(pth$datapath))
    if (!endsWith(pth$datapath, ".shp")) {
      isolate(shapeFile_path(""))
      wrong_file_modal(".shp")
    } else {
      shapeFile_path( pth$datapath )
      shapeFile_path_action(shapeFile_path_action() + 1)
    }
  })

  # Deleting instat file path when removing plot r-instat check box selection
  observeEvent(input$plot_rinstat, {
    if (!input$plot_rinstat) {
      instat_path("")
    }
  })

  # Updating the path to the instat file. (local)
  observeEvent(input$instat_file_local, {
    shinyjs::disable("instat_file_local")
    res <- try(instat_path(file.choose(new = TRUE)))
    if (class(res) != "try-error") {
      if (!endsWith(instat_path(), ".RData")) {
        isolate(instat_path(""))
        wrong_file_modal(".RData")
      } else {
        instat_path_action(instat_path_action() + 1)
      }
    }
    shinyjs::enable("instat_file_local")
  }, ignoreInit = TRUE)

  # Observing changes in selected instat file. (remote)
  shinyFiles::shinyFileChoose(input, 'instat_file_remote', session = session, roots = remoteVolume, filetypes=c('RData'))

  observeEvent(input$instat_file_remote, {
    pth <- shinyFiles::parseFilePaths(remoteVolume,input$instat_file_remote)
    req(nrow(pth) > 0)
    req(file.exists(pth$datapath))
    if (!endsWith(pth$datapath, ".RData")) {
      isolate(instat_path(""))
      wrong_file_modal(".RData")
    } else {
      instat_path( pth$datapath )
      instat_path_action(instat_path_action() + 1)
    }
  })

  resetToPreparePanel <- function() {
    # Clear mem
    rm(list = ls())
    gc()

    # First toggle states of main output panel.
    shinyjs::hide("panel_analyzeGo")
    shinyjs::hide("panel_visualizeGo")
    shinyjs::hide("panel_prepareInput1")
    shinyjs::hide("panel_prepareInput2")
    shinyjs::reset("panel_prepareInput1")
    shinyjs::reset("panel_prepareInput2")
    shinyjs::hide("spinner_prepare1")
    shinyjs::hide("spinner_prepare2")
    shinyjs::hide("spinner_prepare3")
    shinyjs::hide("spinner_analyze")
    shinyjs::hide("spinner_visualize")
    shinyjs::hide("panel_analyze")
    shinyjs::reset("panel_analyze")

    untarVals(list())

    # Resetting data frame
    operatorDataFrame <<- data.frame()
    operatorDataFrameAction(operatorDataFrameAction() + 1)
    shinyjs::show("panel_prepareGo", anim = TRUE, animType = "fade")
  }

  resetToAnalyzePanel <- function() {
    # Clear mem
    rm(list = ls())
    gc()

    # First toggle states of main output panel.
    shinyjs::hide("panel_prepareGo")
    shinyjs::hide("panel_visualizeGo")
    shinyjs::hide("panel_prepareInput1")
    shinyjs::hide("panel_prepareInput2")
    shinyjs::reset("panel_prepareInput1")
    shinyjs::reset("panel_prepareInput2")
    shinyjs::hide("spinner_visualize")
    shinyjs::hide("panel_analyze")
    shinyjs::reset("panel_analyze")
    # Resetting data frame
    operatorDataFrame <<- data.frame()
    operatorDataFrameAction(operatorDataFrameAction() + 1)
    shinyjs::show("panel_analyzeGo", anim = TRUE, animType = "fade")
  }

  resetToVisualizePanel <- function() {
    # Clear mem
    rm(list = ls())
    gc()

    # First toggle states of main output panel.
    shinyjs::hide("panel_prepareGo")
    shinyjs::hide("panel_analyzeGo")
    shinyjs::hide("panel_prepareInput1")
    shinyjs::hide("panel_prepareInput2")
    shinyjs::reset("panel_prepareInput1")
    shinyjs::reset("panel_prepareInput2")
    shinyjs::hide("panel_analyze")
    shinyjs::reset("panel_analyze")
    shinyjs::hide("spinner_visualize")
    shinyjs::hide("visualizePage")
    shinyjs::reset("visualizePage")
    shinyjs::reset("myImage_monitorClimate")
    shinyjs::reset("monitorClimate_PNG")
    shinyjs::reset("monitorClimate_MP4")

    readyToPlot(FALSE)

    # Resetting data frame
    operatorDataFrame <<- data.frame()
    operatorDataFrameAction(operatorDataFrameAction() + 1)
    shinyjs::show("panel_visualizeGo")
    shinyjs::show("setupPage", anim = TRUE, animType = "fade")

  }

  # THIS HAD FLAG napp() == 1.
  observeEvent(input$prepare, {
    resetToPreparePanel()
  })

  # THIS HAD FLAG napp() == 2.
  observeEvent(input$analyze, {
    resetToAnalyzePanel()
  })

  # THIS HAD FLAG napp() == 3.
  observeEvent(input$visualize, {
    resetToVisualizePanel()
  })

  #### Handling the scripts ####
  #### PREPARATION ####
  observeEvent({
    tar_path()
    tar_path_action()
  }, {
    # Requirements
    req(tar_path())
    req(tar_path_action())

    # If wrong format alert and stop.
    #for(path in tar_path()) {
    if (!endsWith(tar_path(), ".tar")) {
      isolate(tar_path(""))
      showModal(modalDialog(
        h4("Wrong file format. Please choose .tar file to continue."),
        title = "Warning!",
        size = "m"
      ))
    } else {
      # Show Spinner
      shinyjs::hide(id = "panel_prepareGo")
      shinyjs::show("spinner_prepare1", anim = TRUE, animType = "fade")

      # Extracting date range and lat/lon range from functions
      extractedDates <- extractDateRange(tar_path(), tar_flag = 1)
      timestep(extractedDates$timestep)

      # Rendering date range.
      output$dateRange_ui <- renderUI({
        dateRangeInput(inputId = "dateRange_prepare",
                       label = "Please select a date range to analyze.",
                       start = extractedDates$date_from,
                       end = extractedDates$date_to,
                       min = extractedDates$date_from,
                       max = extractedDates$date_to,
                       startview = "year"
        )
      })
      shinyjs::hide("spinner_prepare1")
      shinyjs::show(id = "panel_prepareInput1", anim = TRUE, animType = "fade")
    }
  })

  observeEvent(input$dateRange_prepare, {
    req(input$dateRange_prepare[1])
    req(input$dateRange_prepare[2])
    shinyjs::enable("untarAndUnzip")
  })

  # Observing upload of aux file.
  observeEvent(input$aux_upload, {
    res <- try(globalAuxFilePath(file.choose(new = TRUE)))
    if (class(res) != "try-error") {
      shiny::removeModal()
    } else {
      showModal(modalDialog(
        h4("Something went wrong while uploading the auxiliary file. Please try again or choose another .tar file."),
        title = "Error.",
        size = "l"
      ))
    }
  })

  # Observing
  observeEvent(input$aux_download, {
    auxfile <- file.path(userDir, "claas2_level2_aux_data.nc")
    res <- tryCatch(utils::download.file("https://public.cmsaf.dwd.de/data/perm/auxilliary_data/claas2_level2_aux_data.nc", auxfile, "auto", mode = "wb"))

    # Download file returns 0 on success.
    if (class(res) != "try-error" && res == 0) {
      globalAuxFilePath(file.path(auxfile))
      shiny::removeModal()
    } else {
      showModal(modalDialog(
        h4("Something went wrong while downloading the auxiliary file. Please try again or choose another .tar file."),
        title = "Error.",
        size = "l"
      ))
    }
  })

  # Observing canceling of aux file choice. Return to prepare
  observeEvent(input$aux_cancel, {
    shiny::removeModal()
    shinyjs::hide("spinner_prepare2")
    resetToPreparePanel()
  })

  #### Function to untar and unzip files, and give variable names. ####
  untarFiles <- function(path_to_tar, startDate, endDate, timestep, tar_flag = 1) {
    # Get important values.

    tarlist_all <- getTarList(path_to_tar, tar_flag = tar_flag, includeClaasAux = TRUE)

    # WARNING: DATES ARE IN SAME FORMAT (INDEPENDENT OF TIMESTEP ==/!= 'm')
    # IN FACT: WE ARE OMITTING TIMESTEP VALUE CAUSE WE DON'T APPEAR TO NEED IT.
    dates <- extractAllDates(path_to_tar, tar_flag = tar_flag)
    dates <- dates$allDates

    # Get claas_flag and strop from tarlist if needed.
    claas_flag <- getClaasAuxFlag(tarlist_all)
    if (claas_flag) {
      tarlist_all <- tarlist_all[tarlist_all != "CM_SAF_CLAAS2_L2_AUX.nc"]
    }

    if (tar_flag == 0) {
      tar_chunk_size <- 100
    }else{
      tar_chunk_size <- 1000
    }

    # So filelist just equals tarlist here?
    filelist <- NULL

    ordname <- basename(path_to_tar)
    ordpath <- dirname(path_to_tar)

    flist <- list.files(ordpath, substr(ordname, 1, 8), full.names = TRUE)


    for (file in flist) {
      if (tar_flag == 1) {
        tarlist <- utils::untar(file, list = TRUE, tar = "internal")
      } else {
        tarlist <- utils::untar(file, list = TRUE)
      }

      if ("CM_SAF_CLAAS2_L2_AUX.nc" %in% tarlist) {
        tarlist <- subset(tarlist, !(tarlist %in% c("CM_SAF_CLAAS2_L2_AUX.nc")))
        claas_flag <- 1
      }
      tarlist <- sort(tarlist)

      dums <- NA
      dume <- NA

      if (startDate %in% dates) {
        dums <- which(dates == startDate)
      } else {
        if (startDate < dates[1] & endDate >= dates[1])
          (dums <- 1)
      }

      if (endDate %in% dates) {
        dume <- which(dates == endDate)
      } else {
        if (endDate > dates[length(dates)])
          (dume <- length(dates))
      }

      dums <- dums[1]
      dume <- dume[length(dume)]

      if (!is.na(dums)) {
        start <- dums
        end <- dume

        if (length(start) > 1) {
          start <- start[1]
          dums <- start
        }
        if (length(end) > 1) {
          end <- end[length(end)]
          dume <- end
        }

        if (dums > dume) {
          start <- dume
          end <- dums
        }

        tarlist <- tarlist[which(tarlist %in% tarlist_all[start:end])]
        if (length(tarlist) == 0) next

        filelist <- append(filelist, tarlist)

        if (length(tarlist) > tar_chunk_size) {
          dum <- seq(0, length(tarlist), tar_chunk_size)

          for (j in 1:(length(dum) - 1)) {
            tarlist_alt <- tarlist[(dum[j] + 1):dum[j + 1]]
            if (tar_flag == 1) {
              utils::untar(
                file,
                files = tarlist_alt,
                exdir = ordpath,
                tar = "internal"
              )
            } else {
              utils::untar(
                file,
                files = tarlist_alt,
                exdir = ordpath
              )
            }
          }
          tarlist_alt <- tarlist[(dum[length(dum)] + 1):length(tarlist)]
          if (tar_flag == 1) {
            utils::untar(
              file,
              files = tarlist_alt,
              exdir = ordpath,
              tar = "internal"
            )
          } else {
            utils::untar(
              file,
              files = tarlist_alt,
              exdir = ordpath
            )
          }
        } else {

          if (tar_flag == 1) {
            utils::untar(
              file,
              files = tarlist,
              exdir = ordpath,
              tar = "internal"
            )
          } else {
            utils::untar(
              file,
              files = tarlist,
              exdir = ordpath
            )
          }
        }
      }
    } # end if !is.na(dums)

    # Untar auxiliary data if included
    if (claas_flag == TRUE) {
      if (tar_flag == 1) {
        utils::untar(
          path_to_tar,
          files = "CM_SAF_CLAAS2_L2_AUX.nc",
          exdir = ordpath,
          tar = "internal"
        )
      } else {
        utils::untar(path_to_tar,
                     files = "CM_SAF_CLAAS2_L2_AUX.nc",
                     exdir = ordpath)
      }
    }

    # Unzip data in case of NetCDF3
    gzcheck <- length(unlist(strsplit(tarlist[1], ".nc")))

    if (gzcheck == 2) {
      for (i in seq_along(filelist)) {
        zipfile <- file.path(ordpath, filelist[i])
        R.utils::gunzip(zipfile, overwrite = TRUE)
      }

      # Get information of first file

      infile <- unlist(strsplit(filelist[1], ".gz"))
    } else {
      infile <- filelist[1]
    }

    return(
      list(infile =  file.path(ordpath, infile),
           claas_flag = claas_flag,
           filelist = filelist)
    )
  }

  # Create modal for

  getUserOptions <- function(infile, claas_flag) {
    id <- ncdf4::nc_open(infile)
    vn <- names(id$var)
    dn <- names(id$dim)
    lon_var <- "lon"
    lat_var <- "lat"

    '%ni%' <- Negate('%in%')

    if ("lon" %ni% dn) {
      if ("longitude" %in% dn) {
        lon_var <- "longitude"
      }
    }

    if ("lat" %ni% dn) {
      if ("latitude" %in% dn) {
        lat_var <- "latitude"
      }
    }

    if (lon_var %in% c(dn, vn)) {
      lon_range <- range(ncdf4::ncvar_get(id, lon_var), na.rm = TRUE)
    }
    if (lat_var %in% c(dn, vn)) {
      lat_range <- range(ncdf4::ncvar_get(id, lat_var), na.rm = TRUE)
    }
    ncdf4::nc_close(id)

    var_default <-
      subset(vn, !(
        vn %in% c(
          "lat",
          "lon",
          "time_bnds",
          "nb2",
          "time",
          "SATID",
          "latitude",
          "longitude"
        )
      ))

    # Get max level.
    max_level <- NA
    for (variable in vn) {
      if (startsWith(variable, "HLW") || startsWith(variable, "HSH")) {
        id <- ncdf4::nc_open(infile)
        max_level <- length(ncdf4::ncvar_get(id, "lev"))
        ncdf4::nc_close(id)
      }
    }

    # Stop if data are in sinusoidal projection
    if ("sinusoidal" %in% vn) {
      showModal(modalDialog(
        h4("Sorry, the CM SAF R Toolbox can not handle data in sinusoidal projection.
            Please use the 'change projection' option during the order process.
            Your NetCDF data have to be on a regular lon-lat grid."),
        title = "Sorry!",
        size = "l"
      ))

      req(FALSE)
      resetToPreparePanel()
    }

    return(list(
      variables = vn,
      dimensions = dn,
      lat_range = lat_range,
      lon_range = lon_range,
      max_level = max_level
    ))
  }

  # When untar button is pressed.
  observeEvent(input$untarAndUnzip, {
    # Require positve click value.
    req(input$untarAndUnzip > 0)

    # Get first of month if timestep == m
    if (timestep() == "m") {
      dateRange_prep(firstOfMonth(input$dateRange_prepare))
    } else {
      dateRange_prep(input$dateRange_prepare)
    }

    # Show spinner
    shinyjs::hide("panel_prepareInput1")
    shinyjs::reset("panel_prepareInput1")
    shinyjs::show("spinner_prepare2", anim = TRUE, animType = "fade")

    untarVals(untarFiles(tar_path(), dateRange_prep()[1], dateRange_prep()[2], timestep(), tar_flag = 1))
  }, ignoreInit = TRUE)

  # Set all input values for next stage.
  observe({
    # First do aux check
    req(untarVals()$infile)
    req(!is.null(untarVals()$claas_flag))

    ordPath <- dirname(untarVals()$infile)

    infile <- untarVals()$infile
    claas_flag <- untarVals()$claas_flag
    id <- ncdf4::nc_open(infile)
    file_info <- cmsafops:::check_dims(id)
    ncdf4::nc_close(id)

    if (!file_info$has_lon_lat) {
      if (claas_flag) {
        auxFilePath(file.path(ordPath, "CM_SAF_CLAAS2_L2_AUX.nc"))
        cmsafops::add_grid_info(infile, auxFilePath(), outfile = NULL, overwrite = TRUE)
      } else {
        grid_info <- get_grid(infile)
        if (grid_info == 5 && (is.null(globalAuxFilePath()) || !file.exists(globalAuxFilePath()))) {
          showModal(modalDialog(
            h4("Your data seems to require an auxiliar file. You can either upload a local auxiliar file or download it from the public CM SAF website."),
            br(),
            fluidRow(column(6, actionButton(inputId = "aux_upload",
                                            label = "Upload aux file.",
                                            class = "btn btn-success btn-lg btn-block")),
                     column(6, actionButton(inputId = "aux_download",
                                            label = "Download aux file.",
                                            class = "btn btn-success btn-lg btn-block"))),
            title = "Auxiliary data needed.",
            footer = actionButton(inputId = "aux_cancel",
                                  label = "Cancel"),
            size = "l"
          ))

          # Leave silently and come back when auxFilePath is updated.
          req(FALSE)
          cmsafops::add_grid_info(infile, globalAuxFilePath(), outfile = NULL, overwrite = TRUE)
        } else if (grid_info == 2 || grid_info == 7) {
          showModal(modalDialog(
            h4("Sorry, the CM SAF R Toolbox can not handle data in sinusoidal projection.
               Please use the 'change projection' option during the order process.
               Your NetCDF data have to be on a regular lon-lat grid."),
            title = "Error!",
            size = "l"))

          resetToPreparePanel()
          req(FALSE)
        }
      }

    }

    userOptions <- getUserOptions(infile, claas_flag)

    # Set max_level
    if (is.numeric(userOptions$max_level)) {
      max_level(userOptions$max_level)
      shinyjs::show("level_ui")
    } else {
      max_level(0)
      shinyjs::hide("level_ui")
    }



    # Remembering the largest possible range.
    spatialRange$lat_range <- userOptions$lat_range
    spatialRange$lon_range  <- userOptions$lon_range

    # Remembering dimensions
    dimensions(userOptions$dimensions)

    output$variable_ui <- renderUI({
      vars <- subset(userOptions$variables, !(userOptions$variables %in% c("lat", "lon", "time_bnds", "nb2", "time")))
      selectInput("variableInput",
                  "We found the following variables",
                  choices = vars)
    })

    # Rendering lat, lon range.
    output$lonRange_ui <- renderUI({
      sliderInput(inputId = "lonRange",
                  label = "Please select a longitude range.",
                  min = trunc(20 * userOptions$lon_range[1]) / 20,
                  max = trunc(20 * userOptions$lon_range[2]) / 20,
                  value = c(trunc(20 * userOptions$lon_range[1]) / 20, trunc(20 * userOptions$lon_range[2]) / 20),
                  step = 0.05
      )
    })

    # Rendering lat, lat range.
    output$latRange_ui <- renderUI({
      sliderInput(inputId = "latRange",
                  label = "Please select a latitude range.",
                  min = trunc(20 * userOptions$lat_range[1]) / 20,
                  max = trunc(20 * userOptions$lat_range[2]) / 20,
                  value = c(trunc(20 * userOptions$lat_range[1]) / 20, trunc(20 * userOptions$lat_range[2]) / 20),
                  step = 0.05
      )
    })

    # Reset auxFilePath and untarVals
    untarVals()
    auxFilePath()

    # Show second part of prepare and hide spinner
    shinyjs::hide("spinner_prepare2")
    shinyjs::show(id = "panel_prepareInput2", anim = TRUE, animType = "fade")
  })

  # Creating a preview plot.
  output$previewSpatialCoveragePlot <- renderPlot({
    req(input$lonRange)
    req(input$latRange)

    cmsafvis::render_preview_plot(spatial_lon_range = isolate(spatialRange$lon_range),
                                  spatial_lat_range = isolate(spatialRange$lat_range),
                                  lonRange = input$lonRange,
                                  latRange = input$latRange)
  })

  observe({
    req(input$variableInput)
    req(max_level())

    if (startsWith(input$variableInput, "HLW") | startsWith(input$variableInput, "HSH")) {
      output$level_ui <- renderUI({
        numericInput("level",
                     paste0("Select level (1 - ", max_level(), ")"),
                     min = 1,
                     max = max_level(),
                     value = 1,
                     step = 1)
      })
      shinyjs::show("level_ui")
      usingLevel(TRUE)
    } else {
      shinyjs::hide("level")
      usingLevel(FALSE)
    }
  })

  observeEvent(input$level, {
    shinyjs::disable("createOutput")
    req(is.numeric(input$level))
    if (input$level < 1 || input$level > max_level()) {
      shinyjs::disable("createOutput")
    } else {
      shinyjs::enable("createOutput")
    }
  })

  # Function to create the output
  creatingOutputfile <- function(path_to_tar, startDate, endDate, lon_var, lat_var, var, level, dn, outputFormat, claas_flag, deleteExtracted) {

    filelist <- untarVals()$filelist
    ordDir <- dirname(path_to_tar)

    # Regrid AOD or CLAAS data
    if (claas_flag) {
      cat("Data will be regridded to rectangular grid...", "\n")
      # Regrid first file
      infile  <- filelist[1]
      infile  <- file.path(ordDir, infile)
      auxfile <- file.path(ordDir, "CM_SAF_CLAAS2_L2_AUX.nc")
      dxy <- 0.05     # Target grid resolution

      id <- ncdf4::nc_open(infile)
      rdata <- ncdf4::ncvar_get(id, var)
      ncdf4::nc_close(id)

      id <- ncdf4::nc_open(auxfile)
      lon_reg <- ncdf4::ncvar_get(id, lon_var)
      lat_reg <- ncdf4::ncvar_get(id, lat_var)
      ncdf4::nc_close(id)

      lon_org <- seq(-90.0, 90.0, dxy)
      lat_org <- lon_org

      lon <-
        lon_org[which(lon_org > (min(lon_reg, na.rm = TRUE) - dxy) &
                        lon_org < (max(lon_reg, na.rm = TRUE) + dxy))]
      lat <-
        lat_org[which(lat_org > (min(lat_reg, na.rm = TRUE) - dxy) &
                        lat_org < (max(lat_reg, na.rm = TRUE) + dxy))]

      # FNN solution
      lon_reg   <- as.vector(lon_reg)
      lat_reg   <- as.vector(lat_reg)
      lon_reg2   <- lon_reg[!is.na(lon_reg)]
      lat_reg2   <- lat_reg[!is.na(lon_reg)]

      fnn_a <- FNN::get.knnx(lon, lon_reg2, k = 1)
      fnn_b <- FNN::get.knnx(lat, lat_reg2, k = 1)

      filelist_claas <- NULL

      for (i in seq_along(filelist)) {
        infile  <- filelist[i]
        infile  <- file.path(ordDir, infile)

        id <- ncdf4::nc_open(infile)
        rdata <- as.vector(ncdf4::ncvar_get(id, var))
        ncdf4::nc_close(id)

        rdata2 <- rdata[!is.na(lon_reg)]
        arr_new <- array(NA, c(length(lon), length(lat)))
        arr_new[cbind(fnn_a$nn.index, fnn_b$nn.index)] <- rdata2

        # Put new data into file

        t_name <- "time"
        t_standard_name <- "time"
        t_calendar <- "standard"

        lat_name <- "latitude"
        lat_standard_name <- "latitude"
        lat_long_name <- "latitude"
        lat_units <- "degrees_north"
        lat_axis <- "Y"

        lon_name <- "longitude"
        lon_standard_name <- "longitude"
        lon_long_name <- "longitude"
        lon_units <- "degrees_east"
        lon_axis <- "X"

        info <- "Created with the CM SAF R Toolbox."
        var_prec <- "float"

        # Originally called nc34 and set in header as nc34 <- 3
        # After that no change... I'm now passing this as a shiny select option.
        # outputFormat = "NetCDF3"

        if (outputFormat == "NetCDF4") {
          nc_format <- as.logical(1)
          compression <- 4
        } else {
          nc_format <- as.logical(0)
          compression <- NA
        }

        v_missing_value <- -999

        target <- array(NA, c(length(lon), length(lat), 1))
        target[, , 1] <- arr_new
        target[is.na(target)] <- v_missing_value

        id <- ncdf4::nc_open(infile)
        v_units         <- ncdf4::ncatt_get(id, var, "units")$value
        v_standard_name <- ncdf4::ncatt_get(id, var, "standard_name")$value
        v_long_name     <- ncdf4::ncatt_get(id, var, "long_name")$value
        time1           <- ncdf4::ncvar_get(id, "time")
        t_units         <- ncdf4::ncatt_get(id, "time", "units")$value
        ncdf4::nc_close(id)

        outfile <- file.path(userDir, paste0("RG_", filelist[i]))

        x <- ncdf4::ncdim_def(name = "lon",
                              units = lon_units,
                              vals = lon)
        y <- ncdf4::ncdim_def(name = "lat",
                              units = lat_units,
                              vals = lat)
        t <-
          ncdf4::ncdim_def(
            name = "time",
            units = t_units,
            vals = time1,
            unlim = TRUE
          )

        var1 <-
          ncdf4::ncvar_def(
            name = var,
            units = v_units,
            dim = list(x, y, t),
            missval = v_missing_value,
            prec = var_prec,
            compression = compression
          )

        vars <- list(var1)

        ncnew <- ncdf4::nc_create(outfile, vars, force_v4 = nc_format)

        ncdf4::ncvar_put(ncnew, var1, target)

        ncdf4::ncatt_put(ncnew, var, "standard_name", v_standard_name, prec = "text")
        ncdf4::ncatt_put(ncnew, var, "long_name", v_long_name, prec = "text")

        ncdf4::ncatt_put(ncnew, "time", "standard_name", t_standard_name, prec =
                           "text")
        ncdf4::ncatt_put(ncnew, "time", "calendar", t_calendar, prec = "text")

        ncdf4::ncatt_put(ncnew, "lon", "standard_name", lon_standard_name, prec =
                           "text")
        ncdf4::ncatt_put(ncnew, "lon", "long_name", lon_long_name, prec = "text")
        ncdf4::ncatt_put(ncnew, "lon", "axis", lon_axis, prec = "text")

        ncdf4::ncatt_put(ncnew, "lat", "standard_name", lat_standard_name, prec =
                           "text")
        ncdf4::ncatt_put(ncnew, "lat", "long_name", lat_long_name, prec = "text")
        ncdf4::ncatt_put(ncnew, "lat", "axis", lat_axis, prec = "text")

        ncdf4::ncatt_put(ncnew, 0, "Info", info, prec = "text")

        ncdf4::nc_close(ncnew)

        filelist_claas <-
          append(filelist_claas, paste0("RG_", filelist[i]))
      } # end for filelist

      # Remove original files
      file.remove(file.path(ordDir, filelist))
      file.remove(auxfile)
      filelist <- filelist_claas
    } # end if claas flag

    if (!is.null(level)) {
      pattern <- substr(filelist[1], 1, 5)

      outfile <- file.path(outputDir, paste0(var, "_", startDate, "-", endDate, ".nc"))

      cmsafops::levbox_mergetime(var,
                              level = level,
                              path = ordDir,
                              pattern = pattern,
                              outfile = outfile,
                              lon1 = lon_var[1],
                              lon2 = lon_var[2],
                              lat1 = lat_var[1],
                              lat2 = lat_var[2],
                              nc34 = outputFormat,
                              overwrite = TRUE)
    } else {
      # box_mergetime
      pattern <- substr(filelist[1], 1, 5)

      outfile <- file.path(outputDir, paste0(var, "_", startDate, "-", endDate, ".nc"))

      cmsafops::box_mergetime(var,
                           path = ordDir,
                           pattern = pattern,
                           outfile = outfile,
                           lon1 = lon_var[1],
                           lon2 = lon_var[2],
                           lat1 = lat_var[1],
                           lat2 = lat_var[2],
                           nc34 = outputFormat,
                           overwrite = TRUE)
    }

    # Clean up
    if (deleteExtracted) {
      gzcheck <- length(unlist(strsplit(filelist[1], ".nc")))
      if (gzcheck == 2) {
        removelist <- unlist(strsplit(filelist, ".gz"))
      } else {
        removelist <- filelist
      }

      file.remove(file.path(ordDir, removelist))
    }

    # Clean up variables
    var_list_default <- c("checkstring", "userDir", "date_from", "date_to", "dates",
                          "dates_all", "delete", "dume", "dummy", "dums", "end", "endDate",
                          "filelist", "flist", "gzcheck", "i", "id", "infile", "infile_name",
                          "n", "orddir", "ordname", "ordpath",
                          "outputDir", "pattern", "slash", "split_path", "start",
                          "startDate", "tar_flag", "tarlist", "tarlist_all", "timestep", "var",
                          "var_default", "vn", "zipfile", "t", "claas_flag", "sn", "outputFormat",
                          "lon_var", "lat_var", "%ni%")
    var_list <- ls()

    var_list <- var_list[var_list %in% var_list_default]
    var_list <- append(var_list, c("var_list_default", "var_list"))

    rm(list = var_list)
    return(outfile)
  }

  observeEvent(input$createOutput, {
    # Show spinner
    shinyjs::hide("panel_prepareInput2")
    shinyjs::show("spinner_prepare3", anim = TRUE, animType = "fade")

    # Require positive click value
    req(input$createOutput > 0)
    req(dateRange_prep())

    # Get level or set to NULL.
    if (usingLevel()) {
      level <- input$level
    } else {
      level <- NULL
    }

    # Finally create the output file and remember it's location.
    res <- try(outputFilepath(creatingOutputfile(tar_path(),
                                                 dateRange_prep()[1],
                                                 dateRange_prep()[2],
                                                 input$lonRange,
                                                 input$latRange,
                                                 input$variableInput,
                                                 level,
                                                 dimensions(),
                                                 input$outputFormat,
                                                 FALSE,
                                                 input$deleteExtracted)))

    if (class(res) == "try-error") {
      showModal(modalDialog(
        h4("An error occured while creating an output file."),
        tags$p(paste0("Message: ", res)),
        title = "Error!",
        size = "m"
      ))
    }

    # Variable 'isRunningLocally' can be found in global.R
    if (repeatWarning() && !isRunningLocally) {
      showModal(modalDialog(
        tags$p("A .nc file has been created for you. ",
               "Its contents are temporarily stored in the session folder.",
               "Please downlaod the session folder if you want to save it.",
               "All files will be removed after closing this app!"),
        checkboxInput("noRepeat", "Do not show this message again."),
        title = "Warning to prevent data loss!",
        size = "l"
      ))
    }

    shinyjs::hide("spinner_prepare3")
    resetToAnalyzePanel()
  }, ignoreInit = TRUE)

  observeEvent(input$noRepeat, {
    repeatWarning(!input$noRepeat)
  })

  # Download the created session directory.
  output$download <- downloadHandler(
    filename = function() {
      paste0(sessionName, ".tar")
    },
    content = function(con) {
      if (dir.exists(userDir)) {
        tarname <- paste0(sessionName, ".tar")
        utils::tar(tarname, userDir)
        file.copy(tarname, con)
      }
    },
    contentType = "application/x-tar"
  )

  #### ANALYZING ####
  # If a file has been generated let user decide if they want to continue with
  # this file or select another file.
  observeEvent(outputFilepath(), {
    if (endsWith(outputFilepath(), ".nc")) {
      output$ncFile_analyze <- renderUI({
        tags$pre("We prepared the following .nc file for you: ",
                 basename(outputFilepath()))
      })
      output$ncFile_visualize <- renderUI({
        tags$pre("We prepared the following .nc file for you: ",
                 basename(outputFilepath()))
      })
      shinyjs::show("ncFile_analyze")
      shinyjs::show("useOutputFile_analyze")
      shinyjs::show("or_analyze")
      shinyjs::show("ncFile_visualize")
      shinyjs::show("useOutputFile_visualize")
      shinyjs::show("or_visualize")
    } else {
      shinyjs::hide("ncFile_analyze")
      shinyjs::hide("useOutputFile_analyze")
      shinyjs::hide("or_analyze")
      shinyjs::hide("ncFile_visualize")
      shinyjs::hide("useOutputFile_visualize")
      shinyjs::hide("or_visualize")
    }
  })

  # Starting analyze page.
  observeEvent(nc_path_analyze_action(), {
    # Requirements
    req(nc_path_analyze())

    # If wrong format alert and stop.
    if (!endsWith(nc_path_analyze(), ".nc")) {
      isolate(nc_path_analyze(""))
      showModal(modalDialog(
        h4("Wrong file format. Please choose .nc file to continue."),
        title = "Warning!",
        size = "m"
      ))
    } else {
      # Getting variable(s)
      id <- ncdf4::nc_open(nc_path_analyze())
      vn <- names(id$var)
      dn <- names(id$dim)
      ncdf4::nc_close(id)

      if (!("time" %in% dn)) {
        showModal(modalDialog(
          h4("Sorry, the file you chose does not contain a time dimension."),
          title = "Error!",
          size = "l"))

        resetToPreparePanel()
      } else {

      var_default <- subset(vn, !(vn %in% c("lat", "lon", "time_bnds", "nb2", "time")))

      # Stop if data are in sinusoidal projection
      if ("sinusoidal" %in% vn) {
        showModal(modalDialog(
          h4("Sorry, the CM SAF R Toolbox can not handle data in sinusoidal projection.
           Please use the 'change projection' option during the order process.
           Your NetCDF data have to be on a regular lon-lat grid."),
          title = "Error!",
          size = "l"))

        resetToPreparePanel()
      } else {

      output$usedVariable <- renderUI({
        selectInput("usedVariable",
                    label = "Please choose a variable",
                    choices = var_default,
                    width = "320px")
      })
      shinyjs::hide("panel_analyzeGo")
      shinyjs::show("panel_analyze")
      }}
    }
  }, ignoreInit = TRUE)

  # Update Variable
  observeEvent(input$variableAnalyze, {
    req(input$variableAnalyze)

    var_used(input$variableAnalyze)
  }, ignoreInit = TRUE)

  # Update possible choices for operator.
  observe({
    req(input$operatorGroup)

    output$operator <- renderUI({
      # Operator can be found in global.R
      selectInput("operatorInput",
                  label = "Select an operator.",
                  choices = operators[[input$operatorGroup]],
                  width = "320px")
    })
  })

  observe({
    req(input$operatorGroup)
    req(input$operatorInput)

    climate_analysis_ops <- c(
      "absolute_map",
      "anomaly_map",
      "climatology_map",
      "fieldmean_plot",
      "fieldmean_and_anomaly_map"
    )

    countries_choosable <- codes[, "iso3c"]
    names(countries_choosable) <- codes[, "country.name.en"]

    if (input$operatorInput %in% climate_analysis_ops) {
      # If the monitor climate name is chosen, we will visualize right away!
      shinyjs::show("ClimateAnalysisMessage")
      # Update checkboxes to correct value
      updateCheckboxInput(
        session = session,
        inputId = "applyAnother",
        label = "Do you want to apply another operator afterwards?",
        value = FALSE)
      updateCheckboxInput(
        session = session,
        inputId = "instantlyVisualize",
        label = "Do you want to visualize the results right away?",
        value = TRUE)

      # Render countries including EUR, AFR, TOT
      output$select_country <- renderUI({
        selectInput("country",
                  label = "Please select a country",
                  choices = countries_choosable,
                  selected = countries_choosable[which(countries_choosable == "S_A")],
                  multiple = FALSE)
      })
    }

    # Render some UI elements dependent on infile and chosen operator
    if (input$operatorInput == "selyear") {
      nc <- ncdf4::nc_open(isolate(nc_path_analyze()))
      time_info <- cmsafops:::get_date_time(ncdf4::ncvar_get(nc, "time"), ncdf4::ncatt_get(nc, "time", "units")$value)
      years2 <- time_info$years
      ncdf4::nc_close(nc)

      output$years_to_select <- renderUI({
        selectInput("years",
                    label = "Select years",
                    choices = sort(unique(years2)),
                    width = "320px",
                    multiple = TRUE)
      })
    }

    if (input$operatorInput %in% c("selperiod", "extract.period", climate_analysis_ops)) {
      nc <- ncdf4::nc_open(isolate(nc_path_analyze()))
      date_time <- as.Date(cmsafops::get_time(ncdf4::ncatt_get(nc, "time", "units")$value, ncdf4::ncvar_get(nc, "time")))
      ncdf4::nc_close(nc)

      if (input$operatorInput %in% c("selperiod", "extract.period")) {
        output$dateRange_to_select <- renderUI({
          dateRangeInput("dateRange_analyze",
                         label = "Select date range",
                         start = min(date_time),
                         end = max(date_time),
                         min = min(date_time),
                         max = max(date_time),
                         width = "320px")
        })
      } else {
        # In monitor climate want to initialize in same year.
        output$dateRange_to_select <- renderUI({
          dateRangeInput("dateRange_analyze",
                         label = "Select date range",
                         start = min(date_time[format(date_time, "%Y") == format(max(date_time), "%Y")]),
                         end = max(date_time),
                         min = min(date_time),
                         max = max(date_time),
                         width = "320px")
        })
      }

    }

    if (input$operatorInput %in% c("anomaly_map", "climatology_map", "fieldmean_plot", "fieldmean_and_anomaly_map")) {
      output$climatology_years <- renderUI({

        years <- unique(format(date_time, format = "%Y"))
        if (length(years) == 1) {
          end_year <- years[1]
        } else {
          end_year <- years[length(years) - 1]
        }

        tags$div(
          fluidRow(
            column(width = 5,
                   selectInput("climate_year_start",
                               label = "Climatology start year",
                               choices = years,
                               selected = years[1])),
            column(width = 5,
                   selectInput("climate_year_end",
                               label = "Climatology end year",
                               choices = years,
                               selected = end_year))
          )
        )
      })
    }

    if (input$operatorInput == "seltime") {
      nc <- ncdf4::nc_open(isolate(nc_path_analyze()))
      time_info <- cmsafops:::get_date_time(ncdf4::ncvar_get(nc, "time"), ncdf4::ncatt_get(nc, "time", "units")$value)
      times2 <- time_info$times
      ncdf4::nc_close(nc)

      output$times_to_select <- renderUI({
        selectInput("times",
                    label = "Select times",
                    choices = sort(unique(times2)),
                    width = "320px",
                    multiple = TRUE)
      })
    }

    if (input$operatorInput %in% c("sellonlatbox", climate_analysis_ops)) {
      nc <- ncdf4::nc_open(isolate(nc_path_analyze()))
      lon <- ncdf4::ncvar_get(nc, "lon")
      lat <- ncdf4::ncvar_get(nc, "lat")
      ncdf4::nc_close(nc)

      output$region_to_select <- renderUI({
        tags$div(id = "region",
                 sliderInput("lonRegionSlider",
                             label = "Select longitude",
                             min = ceiling(min(lon, na.rm = T)*100)/100,
                             max = floor(max(lon, na.rm = T)*100)/100,
                             value = c(ceiling(min(lon, na.rm = T)*100)/100,
                                       floor(max(lon, na.rm = T)*100)/100),
                             width = "320px"),
                 sliderInput("latRegionSlider",
                             label = "Select latitude",
                             min = ceiling(min(lat, na.rm = T)*100)/100,
                             max = floor(max(lat, na.rm = T)*100)/100,
                             value = c(ceiling(min(lat, na.rm = T)*100)/100,
                                       floor(max(lat, na.rm = T)*100)/100),
                             width = "320px"))
      })
    }

    if (input$operatorInput == "selpoint.multi") {
      nc <- ncdf4::nc_open(isolate(nc_path_analyze()))
      lon <- ncdf4::ncvar_get(nc, "lon")
      lat <- ncdf4::ncvar_get(nc, "lat")
      ncdf4::nc_close(nc)

      updateCheckboxInput(session, "instantlyVisualize", value = FALSE)

      updateSelectInput(session,
                        "format",
                        "Select output format",
                        choices = c("NetCDF4" = 4, "NetCDF3" = 3, "CSV" = 5))

      output$multi_warning <- renderUI({
        tags$div(
          br(),
          p("This function creates multiple output files.
                                    These will be saved in your output directory.
                                    Instant visualization is not possible!", style = "color:red"),
          br())
        })

    } else {
      updateSelectInput(session,
                        "format",
                        "Select output format",
                        choices = c("NetCDF4" = 4, "NetCDF3" = 3))
    }

    # Disable the correct checkboxes
    disable_apply_another_ops <- climate_analysis_ops
    disable_instant_vis_ops <- c(climate_analysis_ops, "selpoint.multi")

    if (input$operatorInput %in% disable_apply_another_ops) {
      shinyjs::disable("applyAnother")
    } else {
      shinyjs::enable("applyAnother")
    }

    if (input$operatorInput %in% disable_instant_vis_ops) {
      shinyjs::disable("instantlyVisualize")
    } else {
      shinyjs::enable("instantlyVisualize")
    }
  })

  observeEvent(input$add_point, {
    chosen_lonPoints(c(chosen_lonPoints(), input$lonPoint))
    chosen_latPoints(c(chosen_latPoints(), input$latPoint))
    updateNumericInput(session, "lonPoint", label = "Select longitude point", value = 0)
    updateNumericInput(session, "latPoint", label = "Select latitude point", value = 0)
    output$chosen_points <- renderTable({
      df <- data.frame(cbind(chosen_lonPoints(), chosen_latPoints()))
      names(df) <- c("lon", "lat")
      df
    })
  })

  ## Remove file choosing
  volumes_output <- c(Home = fs::path_home(), shinyFiles::getVolumes()())
  shinyFiles::shinyFileChoose(input, "ncSecondFileRemote", roots = volumes_output, session = session)

  output$secondFile <- renderPrint({
    if (is.integer(input$ncSecondFileRemote)) {
      cat("No file has been selected.")
    } else {
      file <- shinyFiles::parseFilePaths(volumes_output, input$ncSecondFileRemote)
      second_infile( file$datapath )
      file$datapath
    }
  })

  # Observing checkboxes
  observeEvent(input$applyAnother, {
    if (!input$applyAnother) {
      shinyjs::show("instantlyVisualize")
    } else {
      shinyjs::hide("instantlyVisualize")
    }
  }, ignoreInit = TRUE)

  # Toggle states of input ui element options for the different operators.
  observeEvent(input$operatorInput, {
    req(input$operatorInput)

    # Operator options can be found in global.R
    currentOperatorOption("None")

    for (option in operatorOptions) {
      if (is.element(input$operatorInput, operatorOptionsDict[[option]])) {
        currentOperatorOption(option)
      } else {
        shinyjs::hide(option)
        shinyjs::hide("dateRange_to_select")
        shinyjs::hide("climatology_years")
        shinyjs::hide("accumulateInfile")
        shinyjs::hide("attachToExisting")
        shinyjs::hide("years_to_select")
        shinyjs::hide("region_to_select")
        shinyjs::hide("times_to_select")
        shinyjs::hide("points_to_select")
        shinyjs::hide("select_country")
        shinyjs::hide("plot_format")
      }
    }

    if (currentOperatorOption() != "None") {
      if (currentOperatorOption() == "dateRange") {
        shinyjs::show("dateRange_to_select")
      } else if (currentOperatorOption() == "years") {
        shinyjs::show("years_to_select")
      } else if (currentOperatorOption() == "times") {
        shinyjs::show("times_to_select")
      } else if (currentOperatorOption() == "region") {
        shinyjs::show("region_to_select")
      } else if (currentOperatorOption() == "point.multi") {
        shinyjs::show("points_to_select")
      } else if (currentOperatorOption() == "monitor_climate") {
        shinyjs::show("accumulateInfile")
        shinyjs::show("attachToExisting")
        shinyjs::show("dateRange_to_select")
        if (input$operatorInput != "absolute_map") {
          shinyjs::show("climatology_years")
        }
        shinyjs::show("region_to_select")
        shinyjs::show("plot_format")
        shinyjs::show("select_country")
      } else {
        shinyjs::show(currentOperatorOption())
      }
    }

    if (input$operatorInput == "selpoint.multi") {
      shinyjs::show("add_point")
      shinyjs::show("multi_warning")
      shinyjs::show("chosen_points")
    } else {
      shinyjs::hide("add_point")
      shinyjs::hide("multi_warning")
      shinyjs::hide("chosen_points")
    }

    if (!isRunningLocally && (input$operatorInput %in% c("remap", "cmsaf.add", "cmsaf.sub") || input$attachToExisting)) {
      shinyjs::show("twofiles")
    } else {
      shinyjs::hide("twofiles")
    }

  }, ignoreInit = TRUE)

  # Updating the operator table and toggle panel state.
  observeEvent(operatorDataFrameAction(), {
    if (nrow(operatorDataFrame) > 0) {
      shinyjs::hide("spinner_analyze")
      shinyjs::show("listOfOperators")
    }

    output$appliedOperators <- renderTable({
      operatorDataFrame
    })

    output$ncShortInfo <- renderPrint({
      req(nc_path_analyze())
      cmsafops::ncinfo(nc_path_analyze())
    })

    if (nrow(operatorDataFrame) == 0) {
      shinyjs::hide("listOfOperators")
    }
  }, ignoreInit = TRUE)


  # multi day accu graphic message
  observe({
    shinyjs::hide("multiDayNonAccuGraphic")
    req(input$operatorGroup)
    req(input$operatorGroup == "Climate Analysis")
    req(!is.null(input$accumulateInfile))
    req(input$dateRange_analyze)
    req(input$plot_format)
    req(!any(is.na(input$dateRange_analyze)))

    if (!input$accumulateInfile &&
        input$plot_format == "graphic" &&
        input$dateRange_analyze[1] != input$dateRange_analyze[2]) {
      shinyjs::show("multiDayNonAccuGraphic")
    }
  })

  observeEvent(input$attachToExisting, {
    if (input$attachToExisting) {
      shinyjs::show("attach_warning")
      updateDateRangeInput(session, "dateRange_analyze",
                           label = "Select date range",
                           min = as.Date("1983-01-01"),
                           max = Sys.Date()
                           )
      updateSelectInput(session, "climate_year_start",
                        label = "Climatology start year",
                        choices = as.character(format(as.Date("1983-01-01"), format = "%Y"):format(Sys.Date(), format = "%Y"))
                        )
      updateSelectInput(session, "climate_year_end",
                        label = "Climatology end year",
                        choices = as.character(format(as.Date("1983-01-01"), format = "%Y"):format(Sys.Date(), format = "%Y"))
                        )
    } else {
      shinyjs::hide("attach_warning")
      nc <- ncdf4::nc_open(isolate(nc_path_analyze()))
      date_time <- as.Date(cmsafops::get_time(ncdf4::ncatt_get(nc, "time", "units")$value, ncdf4::ncvar_get(nc, "time")))
      ncdf4::nc_close(nc)
      updateDateRangeInput(session, "dateRange_analyze",
                                          label = "Select date range",
                                          start = min(date_time[format(date_time, "%Y") == format(max(date_time), "%Y")]),
                                          end = max(date_time),
                                          min = min(date_time),
                                          max = max(date_time)
                           )
      years <- unique(format(date_time, format = "%Y"))
      if (length(years) == 1) {
        end_year <- years[1]
      } else {
        end_year <- years[length(years) - 1]
      }

      updateSelectInput(session, "climate_year_start",
                             label = "Climatology start year",
                             choices = years,
                             selected = years[1])
      updateSelectInput(session, "climate_year_end",
                             label = "Climatology end year",
                             choices = years,
                             selected = end_year)

    }
  }, ignoreInit = TRUE)


  # Apply the operator and show details in table.
  observeEvent(input$applyOperator, {
    req(input$operatorInput)
    req(input$usedVariable)

    # Disable till done.
    shinyjs::disable("applyOperator")
    shinyjs::hide("listOfOperators")
    shinyjs::show("spinner_analyze", anim = TRUE, animType = "fade")

    # Update the current operator value.
    if (currentOperatorOption() == "None") {
      currentOperatorValue(NULL)
    } else {
      if (currentOperatorOption() == "dateRange") {
        currentOperatorValue(input[["dateRange_analyze"]])
      } else {
        currentOperatorValue(input[[currentOperatorOption()]])
      }
    }

    # Create a time stamp
    time <- as.numeric(format(Sys.time(), "%H%M%S"))

    # Update outfile path
    newOutfile <- file.path(outputDir, paste0(input$usedVariable, "_", input$operatorInput, time, ".nc"))

    if (newOutfile == nc_path_analyze()) {
      # Adding a star tag at end  to prevent equal input and output file name.
      newOutfile <- file.path(outputDir, paste0(input$usedVariable, "_", input$operatorInput, time, "x.nc"))
    }

    # Here I chose a shorter outfile. Easily revertable.
    # newOutfile <- paste0(outputDir, "/", input$usedVariable,"_Apply.Function_", input$operatorInput, time, ".nc")

    # Run the operator
    if (currentOperatorOption() == "None") {
      argumentList <- list(var = input$usedVariable,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE)
    } else if (currentOperatorOption() == "constant") {
      argumentList <- list(var = input$usedVariable,
                           const = input$constant,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE)
    } else if (currentOperatorOption() == "region") {
      # Sellonlatbox
      argumentList <- list(var = input$usedVariable,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           lon1 = input$lonRegionSlider[1],
                           lon2 = input$lonRegionSlider[2],
                           lat1 = input$latRegionSlider[1],
                           lat2 = input$latRegionSlider[2],
                           nc34 = input$format,
                           overwrite = TRUE)
    } else if (currentOperatorOption() == "point") {
      if (input$operatorInput == "selpoint") {
        argumentList <- list(var = input$usedVariable,
                             infile = nc_path_analyze(),
                             outfile = newOutfile,
                             lon1 = input$lonPoint,
                             lat1 = input$latPoint,
                             nc34 = input$format,
                             overwrite = TRUE)
      } else {
        newOutfile <- nc_path_analyze()
        format <- "nc"
        nc34 <- input$format
        if (input$format == 5) {
          format <- "csv"
          nc34 <- 3
        }
        argumentList <- list(var = input$usedVariable,
                             infile = nc_path_analyze(),
                             outpath = outputDir,
                             lon1 = as.numeric(chosen_lonPoints()),
                             lat1 = as.numeric(chosen_latPoints()),
                             nc34 = nc34,
                             format = format)
        chosen_lonPoints(c())
        chosen_latPoints(c())
      }
    } else if (currentOperatorOption() == "dateRange") {
      argumentList <- list(var = input$usedVariable,
                           start = input$dateRange_analyze[1],
                           end = input$dateRange_analyze[2],
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE)
    } else if (currentOperatorOption() == "useFastTrend") {
      trendValue <- 1
      if (!input$useFastTrend) {
        trendValue <- 2
      }
      argumentList <- list(var = input$usedVariable,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           option = trendValue,
                           nc34 = input$format,
                           overwrite = TRUE)
    } else if (currentOperatorOption() == "percentile") {
      argumentList <- list(var = input$usedVariable,
                           p = input$percentile,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE)
    }else if (currentOperatorOption() == "months") {
      monthList <- which(c("January", "February", "March", "April", "May",
                           "June", "July", "August", "September", "October",
                           "November", "December")
                         %in% input$months)
      argumentList <- list(var = input$usedVariable,
                           month = monthList,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE)
    }else if (currentOperatorOption() == "years") {
      argumentList <- list(var = input$usedVariable,
                           year = input$years,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE)
    }else if (currentOperatorOption() == "times") {
      argumentList <- list(var = input$usedVariable,
                           hour_min = input$times,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE)
    }else if (currentOperatorOption() == "method") {

      # Select second input file depending on local or remote session
      if (!isRunningLocally) {
        infile2 <- second_infile()
      } else {
        infile2 <- try( file.choose(new = TRUE) )
      }

      if (class(infile2) == "try-error") {
        # Show modal and leave silently.
        showModal(modalDialog(
          h4("An additional file is required for this operator."),
          br(),
          h4("The grid information of infile2 are the target grid for the interpolation. This File may also be an ASCII-File containing the grid information."),
          br(),
          h4("For more information please type '?cmsafops::remap' in your R console."),
          title = "No remap file given.",
          size = "l"
        ))

        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")
        req(FALSE)
      } else if (!file.exists(infile2) || !(endsWith(infile2, ".nc") || endsWith(infile2, ".txt"))) {
        # Show modal and leave silently.
        showModal(modalDialog(
          h4("The additional file needs to be a", tags$strong(".nc"), " file (or", tags$strong(".txt"), " in case of ASCII-format)."),
          br(),
          h4("For more information please type '?cmsafops::remap' in your R console."),
          title = "No remap file given.",
          size = "l"
        ))

        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")

        req(FALSE)
      }

      argumentList <- list(var = input$usedVariable,
                           infile1 = nc_path_analyze(),
                           infile2 = infile2,
                           outfile = newOutfile,
                           method = input$method,
                           nc34 = input$format,
                           overwrite = TRUE)
    } else if (currentOperatorOption() == "file_select") {
      # Select second input file depending on local or remote session
      if (!isRunningLocally) {
        infile2 <- second_infile()
      } else {
        infile2 <- try( file.choose(new = TRUE) )
      }

      if (class(infile2) == "try-error") {
        # Show modal and leave silently.
        showModal(modalDialog(
          h4("An additional file is required for this operator."),
          br(),
          h4("If you want to add/subtract a constant value, please select the operator 'Add constant to data'/'Subtract constant from data'"),
          title = "No additional file given.",
          size = "l"
        ))

        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")
        req(FALSE)
      } else if (!file.exists(infile2) || !(endsWith(infile2, ".nc"))) {
        # Show modal and leave silently.
        showModal(modalDialog(
          h4("The additional file needs to be a", tags$strong(".nc"), " file."),
          br(),
          h4("For more information please type '?cmsafops::cmsaf.add' in your R console."),
          title = "File selection error",
          size = "l"
        ))

        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")

        req(FALSE)
      }
      argumentList <- list(vari1 = input$usedVariable,
                           vari2 = input$usedVariable,
                           infile1 = nc_path_analyze(),
                           infile2 = infile2,
                           outfile = newOutfile,
                           nc34 = input$format,
                           overwrite = TRUE)
    } else if (currentOperatorOption() == "monitor_climate") {
      # THE MONITOR CLIMATE FROM CMSAFVIS
      if (input$plot_format == "graphic") {
        fileext <- ".png"
      }  else {
        fileext <- ".mp4"
      }

      #monitor_climate_out_dir <- tempdir()
      monitor_climate_temp_dir <- file.path(outputDir, "mc_temp")
      if (!dir.exists(monitor_climate_temp_dir)) {
        dir.create(monitor_climate_temp_dir)
      }

      monitor_climate_out_dir <- file.path(outputDir)

      monitor_climate_outfile <- paste0(input$usedVariable, "_", input$operatorInput, time, fileext)
      monitor_climate_outfile_path <- file.path(monitor_climate_out_dir, monitor_climate_outfile)

      if (input$attachToExisting) {
      # Select second input file depending on local or remote session
      if (!isRunningLocally) {
        infile_attach <- second_infile()
      } else {
        infile_attach <- try( file.choose(new = TRUE) )
      }

      if (class(infile_attach) == "try-error") {
        # Show modal and leave silently.
        showModal(modalDialog(
          h4("An additional file is required for this operator."),
          br(),
          h4("If you want to add/subtract a constant value, please select the operator 'Add constant to data'/'Subtract constant from data'"),
          title = "No additional file given.",
          size = "l"
        ))

        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")
        req(FALSE)
      } else if (!file.exists(infile_attach) || !(endsWith(infile_attach, ".nc"))) {
        # Show modal and leave silently.
        showModal(modalDialog(
          h4("The additional file needs to be a", tags$strong(".nc"), " file."),
          br(),
          h4("For more information please type '?cmsafvis::monitor_climate' in your R console."),
          title = "File selection error",
          size = "l"
        ))

        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")

        req(FALSE)
      }
      } else {
      infile_attach <- "auto"
    }

      argumentList <- list(
        plot_type = input$operatorInput,
        infile = nc_path_analyze(),
        accumulate = input$accumulateInfile,
        variable = input$usedVariable,
        output_format = input$plot_format,
        animation_pace = input$animation_pace,
        freeze_animation = FALSE,
        lang = "eng",
        outfile_name = monitor_climate_outfile,
        start_date = input$dateRange_analyze[1],
        end_date = input$dateRange_analyze[2],
        country_code = input$country,
        lon_min = input$lonRegionSlider[1],
        lon_max = input$lonRegionSlider[2],
        lat_min = input$latRegionSlider[1],
        lat_max = input$latRegionSlider[2],
        out_dir = monitor_climate_out_dir,
        temp_dir = monitor_climate_temp_dir,
        climate_dir = monitor_climate_temp_dir,
        attach = input$attachToExisting,
        infile_attach = infile_attach
      )

      if (input$operatorInput != "absolute_map") {
        argumentList <- append(
          argumentList,
          list(
            climate_year_start = as.numeric(input$climate_year_start),
            climate_year_end = as.numeric(input$climate_year_end)
          )
        )
      }
    }


    climate_analysis_ops <- c("absolute_map", "anomaly_map", "climatology_map", "fieldmean_plot", "fieldmean_and_anomaly_map")

    # Get package and function
    if (input$operatorInput %in% climate_analysis_ops) {
      fun <- get("monitor_climate", asNamespace("cmsafvis"))
    } else {
      fun <- get(input$operatorInput, asNamespace("cmsafops"))
    }

    res <- try(do.call(fun, argumentList))

    # Error handling
    if (class(res) == "try-error") {
      showModal(modalDialog(
        h4("An error occured while applying the operator."),
        tags$p(paste0("Message: ", res)),
        title = "Error!",
        size = "l"
      ))
    } else {
      shinyjs::hide("add_point")
      # No error. Continue with rest.

      # If monitor climate store png/mp4 paths
      if (input$operatorInput %in% climate_analysis_ops) {
        image_path_visualize( monitor_climate_outfile_path )
        actionVisualizeMonitorClimate(actionVisualizeMonitorClimate() + 1)

        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")
        return()
      } else {
        nc_path_analyze(newOutfile)
        nc_path_visualize(newOutfile)

      }

      # Variable 'isRunningLocally' can be found in global.R
      if (repeatWarning() && !isRunningLocally) {
        showModal(modalDialog(
          tags$p("A .nc file has been created for you. ",
                 "Its contents are temporarily stored in the session folder.",
                 "Please download the session folder if you want to save it.",
                 "All files will be removed after closing this app!"),
          checkboxInput("noRepeat", "Do not show this message again."),
          title = "Warning to prevent data loss!",
          size = "l"
        ))
      }

      if (input$applyAnother) {
        # Depending on operator give option details.
        if (currentOperatorOption() == "None") {
          newRow <- data.frame(input$operatorInput, "None", "None")
        } else if (currentOperatorOption() == "point") {
          newRow <- data.frame(input$operatorInput, "point", paste0("lat: ", input$latPoint, ", lon: ", input$lonPoint))
        } else if (currentOperatorOption() == "region") {
          newRow <- data.frame(input$operatorInput, "region", paste0("lat: [", input$latRegionSlider[1], " ", input$latRegionSlider[2], "], ",
                                                                     "lon: [", input$lonRegionSlider[1], " ", input$lonRegionSlider[2], "]"))
        } else if (currentOperatorOption() == "dateRange") {
          newRow <- data.frame(input$operatorInput, "dateRange", paste0("from ", input$dateRange_analyze[1], " to ", input$dateRange_analyze[1]))
        } else {
          newRow <- data.frame(input$operatorInput, currentOperatorOption(), input[[currentOperatorOption()]])
        }

        lastCols <- data.frame(basename(newOutfile))
        newRow <- cbind(newRow, lastCols)

        # Give the row names.
        names(newRow) <- c("Operator", "Option", "Value", "Output File")
        # Bind row to table
        if (nrow(operatorDataFrame)) {
          operatorDataFrame <<- rbind(operatorDataFrame, newRow)
          operatorDataFrameAction(operatorDataFrameAction() + 1)
        } else {
          operatorDataFrame <<- newRow
          operatorDataFrameAction(operatorDataFrameAction() + 1)
        }
      } else if (input$instantlyVisualize) {
        # Switching to visualize screen.
        actionVisualize(actionVisualize() + 1)
      } else {
        # Remove spinner here and reset to analyze.
        resetToAnalyzePanel()
      }
    }

    # Either way allow to apply another operator and remove spinner.
    shinyjs::hide("spinner_analyze")
    if (input$applyAnother) {
      shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
    }
    shinyjs::enable("applyOperator")

  }, ignoreInit = TRUE)


  observeEvent({
    actionVisualize()
    action_visualize_post_modal()
  },{
    shinyjs::hide("downloadExitMonitorClimate")
    shinyjs::hide("myImage_monitorClimate")
    shinyjs::show("downloadExit")
  })

  observeEvent(actionVisualizeMonitorClimate(), {
    shinyjs::hide("downloadExit")
    shinyjs::show("downloadExitMonitorClimate")
  })

  #### VISUALIZE ####
  getVariableData <- function(timestep_index, id, var) {
    return(ncdf4::ncvar_get(id, var, start = c(1, 1, timestep_index), count = c(-1, -1, 1)))
  }

  # A function to read all required information from nc file
  # that's needed for the visualize options.
  # Also sets the correct image width and height.
  get_visualize_options <- function(infile, var) {
    # Open file and get data
    id <- ncdf4::nc_open(infile)
    # Remap to regGrid if necessary
    file_info <- cmsafops:::check_dims(id)
    ncdf4::nc_close(id)
    if (!file_info$isRegGrid) {
      remap_timestamp <- format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC")
      remap_name <- paste0("remap_", remap_timestamp, ".nc")
      outfile <- file.path(userDir, remap_name)
      # grid_filepath can be  found in global.R
      cmsafops::remap(var, infile, grid_filepath, outfile, overwrite = TRUE)
      infile <- outfile
      nc_path_visualize(infile)
    }

    # Open file and get data
    id <- ncdf4::nc_open(infile)

    lon <- ncdf4::ncvar_get(id, "lon")
    lat <- ncdf4::ncvar_get(id, "lat")

    data <- try(ncdf4::ncvar_get(id, var, collapse_degen = FALSE))

    if (all(is.na(data))) {
      stop("The file you are trying to visualize constains only NA values.")
    }

    visualizeDataTimestep(getVariableData(1, id, var))

    date <- ncdf4::ncvar_get(id, "time")
    t_unit <- ncdf4::ncatt_get(id, "time", "units")$value
    date.time <- as.character(cmsafops::get_time(t_unit, date))
    unit <- ncdf4::ncatt_get(id, var, "units")$value
    if (unit == 0)
      (unit <- "-")
    vn <- var
    varname <- ncdf4::ncatt_get(id, var, "long_name")$value
    if (varname == 0)
      (varname <- ncdf4::ncatt_get(id, var, "standard_name")$value)
    if (varname == 0)
      (varname <- var)

    creator_att <- ncdf4::ncatt_get(id, 0, "creator_name")
    creator <- ifelse(creator_att$hasatt, creator_att$value, "-")
    copyrightText <- paste0("Data Source: ", creator)

    ncdf4::nc_close(id)

    # In HOAPS files problems can occur due to different sorted values
    # check data dimensions

    validData <- (class(data) != "try-error")

    reversedDimensions$transpose  <- FALSE
    reversedDimensions$lonReverse <- FALSE
    reversedDimensions$latReverse <- FALSE

    if (dim(visualizeDataTimestep())[1] != length(lon)) {
      if (dim(visualizeDataTimestep())[2] == length(lon)) {
        reversedDimensions$transpose <- TRUE
        visualizeDataTimestep(aperm(visualizeDataTimestep(), c(2, 1)))
        if (validData) {
          data <- aperm(data, c(2, 1, 3))
        }
      }
    }
    # check longitude
    if (lon[1] > lon[length(lon)]) {
      reversedDimensions$lonReverse <- TRUE
      lon <- rev(lon)

      visualizeDataTimestep(visualizeDataTimestep()[rev(seq_len(length(lon))), ])
      if (validData) {
        if (dim(data)[3] == 1) {
          data[, , 1] <- data[rev(seq_len(length(lon))), , ]
        } else {
          data <- data[rev(seq_len(length(lon))), , ]
        }
      }
    }
    # check latitude
    if (lat[1] > lat[length(lat)]) {
      lat <- rev(lat)
      reversedDimensions$latReverse <- TRUE
      visualizeDataTimestep(visualizeDataTimestep()[, rev(seq_len(length(lat)))])

      if (validData) {
        if (dim(data)[3] == 1) {
          data[, , 1] <- data[, rev(seq_len(length(lat))), ]
        } else {
          data <- data[, rev(seq_len(length(lat))), ]
        }
      }
    }

    # Returning 1D plot information
    if (length(lon) == 1 && length(lat) == 1) {
      min_lon <- lon
      max_lon <- lon
      min_lat <- lat
      max_lat <- lat

      visualizeDataMin(min(data, na.rm = TRUE))
      visualizeDataMax(max(data, na.rm = TRUE))
      x_range <- length(data)
      ltype <- c("l", "p", "o", "s", "h")

      if (startsWith(t_unit, "hours")) {
        date.time <- as.POSIXct(date.time, format = "%Y-%m-%d %R")
      } else {
        date.time <- as.Date(date.time)
      }

      fit <- stats::lm(as.vector(data) ~ c(seq_along(data)))
      dummy <- fit$fitted.values
      if (sum(!is.na(data)) == length(dummy)) {
        fitted <- data
        fitted[which(!is.na(fitted))] <- dummy
        rm(dummy)
      }
      ylabel <- paste0(varname, " [", unit, "]")

      return(
        list(
          plot_dim = 1,
          data = data,
          date.time = date.time,
          x_range = x_range,
          ylabel = ylabel,
          vn = vn,
          varname = varname,
          unit = unit,
          lat = lat,
          lon = lon,
          fitted = fitted,
          copyrightText = copyrightText
        )
      )

    } else {
      # Returning 2D plot information
      min_lon <- min(lon, na.rm = TRUE)
      max_lon <- max(lon, na.rm = TRUE)

      min_lat <- min(lat, na.rm = T)
      max_lat <- max(lat, na.rm = T)

      if (class(data) != "try-error") {
        min_data <- min(data, na.rm = TRUE)
        max_data <- max(data, na.rm = TRUE)

        if (round(min_data, digits = 1) == round(max_data, digits = 1)) {
          min_data <- min_data - 0.05
          max_data <- max_data + 0.05
        }

        visualizeDataMin(min_data)
        visualizeDataMax(max_data)
      }

      return(
        list(
          plot_dim = 2,
          data = data,
          date.time = date.time,
          min_lon = min_lon,
          max_lon = max_lon,
          min_lat = min_lat,
          max_lat = max_lat,
          vn = vn,
          varname = varname,
          unit = unit,
          lat = lat,
          lon = lon,
          copyrightText = copyrightText
        )
      )
    }
  }

  observeEvent(input$action_visualize_variable_modal, {
    # Update the variable
    variable_visualize_modal(input$variable_visualize_modal)

    # This will re-trigger the visualization process
    action_visualize_post_modal(action_visualize_post_modal() + 1)

    # This will remove the modal.
    removeModal()
  })

  # This will set up the intial input values on visualize page.
  observeEvent({
    actionVisualize()
    action_visualize_post_modal()
  }, {
    req(nc_path_visualize())
    shinyjs::hide("panel_visualizeGo")
    shinyjs::show("spinner_visualize", anim = TRUE, animType = "fade")
    shiny::showTab(inputId = "mainVisualizeTabset", target = "Statistics")
    shiny::showTab(inputId = "mainVisualizeTabset", target = "File Summary")

    id <- ncdf4::nc_open(nc_path_visualize())
    vn <- names(id$var)
    dn <- names(id$dim)
    ncdf4::nc_close(id)

    if (!("time" %in% dn)) {
      showModal(modalDialog(
        h4("Sorry, the file you chose does not contain a time dimension."),
        title = "Error!",
        size = "l"))

      resetToPreparePanel()
    } else {

    vn <- subset(vn, !(vn %in% c("lat", "lon", "time_bnds", "nb2", "time")))

    # If more than one we allow user to choose a variable. Catch this input here.
    if (!is.null(variable_visualize_modal())) {
      if (is.element(variable_visualize_modal(), vn)) {
        vn <- variable_visualize_modal()

        variable_visualize_modal(NULL)
      } else {
        showModal(modalDialog(
          h4("The chosen input variable doesn't exist in this file. Try another file or input argument."),
          br(),
          title = "Oops.",
          size = "l"
        ))
      }
    }

    if (length(vn) > 1) {
      showModal(modalDialog(
        h4("Seems like you are trying to visualize a file with multiple variables. Please select a variable in order to continue."),
        br(),
        fluidRow(column(7, selectInput(inputId = "variable_visualize_modal",
                                       label = "Please choose a variable.",
                                       choices = vn)),
                 column(5, actionButton("action_visualize_variable_modal",
                                        "Visualize using this variable.",
                                        width = "100%"))),
        title = "We need your help.",
        size = "l",
        footer = NULL
      ))

      # Leave
      req(FALSE)
    } else if (length(vn) < 1) {
      showModal(modalDialog(
        h4("The chosen file doesn't have a variable."),
        title = "Error!",
        size = "m"
      ))

      # Leave
      req(FALSE)
    } else if (vn == "sinusoidal") {
      # TODO: Change this as soon as sinusodial is done.
      # Stop if data are in sinusoidal projection
      showModal(modalDialog(
        h4("Sorry, the CM SAF R Toolbox can not handle data in sinusoidal projection.
             Please use the 'change projection' option during the order process.
             Your NetCDF data have to be on a regular lon-lat grid."),
        title = "Error!",
        size = "l"
      ))

      # Leave
      req(FALSE)
    } else {
      # Trying. If error go back to Visualize page.
      # Maybe visualizeVariables isn't loaded correctly second time?
      res <- try(visualizeVariables(get_visualize_options(nc_path_visualize(), vn)))

      if (class(res) == "try-error") {
        showModal(modalDialog(
          h3("An error occured while preparing the visualization. Please try another file."),
          br(),
          h5(paste0(res)),
          title = "Error!",
          size = "l"))

        # Go to visualize page.
        resetToVisualizePanel()
      } else {

        shinyjs::hide("setupPage")
        shinyjs::hide("spinner_visualize")
        shinyjs::show("visualizePage", anim = TRUE, animType = "fade")

        # No error. Let's plot!
        # Rendering all gui elements dependent on file.

        # 2D Plot:
        if (visualizeVariables()$plot_dim == 2) {

          shiny::showTab(inputId = "mainVisualizeTabset", target = "Statistics")
          shinyjs::hide("sidebar_1d_plot")
          shinyjs::hide("myImage_1d")
          shinyjs::hide("spinner_visualize")
          shinyjs::hide("filedownload")

          shinyjs::show("myImage_2d")
          shinyjs::show("sidebar_2d_plot")


          output$timestep_visualize <- renderUI({
            selectInput("timestep",
                        label = "Select Time Step",
                        choices = visualizeVariables()$date.time,
                        selected = visualizeVariables()$date.time[1],
                        selectize = FALSE)
          })

          output$lon_visualize <- renderUI({
            tmp <- c(max(round(visualizeVariables()$min_lon), -180), min(round(visualizeVariables()$max_lon), 180))
            lon_bounds(tmp)
            sliderInput("slider1",
                        label = "Longitude",
                        min = max(round(visualizeVariables()$min_lon) - 20, -180),
                        max = min(round(visualizeVariables()$max_lon) + 20, 180),
                        value = c(tmp[1], tmp[2]))
          })

          output$lat_visualize <- renderUI({
            tmp <- c(max(round(visualizeVariables()$min_lat), -90), min(round(visualizeVariables()$max_lat), 90))
            lat_bounds(tmp)

            sliderInput("slider2",
                        label = "Latitude",
                        min = max(round(visualizeVariables()$min_lat) - 20, -90),
                        max = min(round(visualizeVariables()$max_lat) + 20, 90),
                        value = c(tmp[1], tmp[2]))
          })

          output$title_text <- renderUI({
            textInput("text1",
                      label = "Title",
                      value = visualizeVariables()$varname)
          })

          output$subtitle_text <- renderUI({
            textInput("text2",
                      label = "Subtitle",
                      value = " ")
          })

          output$scale_caption <- renderUI({
            textInput("text3",
                      label = "Scale Caption",
                      value = paste0(visualizeVariables()$varname, " [", visualizeVariables()$unit, "]"))
          })
        } else {
          # 1D-Plot

          # Hide Statistics panel
          shiny::hideTab(inputId = "mainVisualizeTabset", target = "Statistics")

          shinyjs::hide("sidebar_2d_plot")
          shinyjs::hide("myImage_2d")

          shinyjs::show("myImage_1d")
          shinyjs::show("sidebar_1d_plot")
          shinyjs::show("filedownload")

          output$x_visualize <- renderUI({
            sliderInput("sliderx",
                        label = "X-Range",
                        min = 1,
                        max = visualizeVariables()$x_range,
                        value = c(1, visualizeVariables()$x_range))
          })

          output$title_text_1d <- renderUI({
            textInput("text1_1d",
                      label = "Title",
                      value = visualizeVariables()$varname)
          })

          output$subtitle_text_1d <- renderUI({
            textInput("text2_1d",
                      label = "Subtitle",
                      value = " ")
          })
        }

        # Start timer independently of 1D/2D plot.
        shinyjs::delay(2000, readyToPlot(TRUE))
      }
    }}

  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # Logic after png or mp4 passed to visualizer
  # This will set up the intial input values on visualize page.
  observeEvent({
    actionVisualizeMonitorClimate()
  }, {
    req(image_path_visualize())
    shinyjs::hide("panel_visualizeGo")
    shinyjs::show("spinner_visualize", anim = TRUE, animType = "fade")

    shinyjs::hide("setupPage")
    shinyjs::hide("spinner_visualize")
    shinyjs::show("visualizePage", anim = TRUE, animType = "fade")

    shiny::hideTab(inputId = "mainVisualizeTabset", target = "Statistics")
    shiny::hideTab(inputId = "mainVisualizeTabset", target = "File Summary")
    shinyjs::hide("sidebar_1d_plot")
    shinyjs::hide("myImage_1d")
    shinyjs::hide("sidebar_2d_plot")
    shinyjs::hide("myImage_2d")
    shinyjs::hide("spinner_visualize")
    shinyjs::hide("filedownload")
    shinyjs::show("myImage_monitorClimate")

    if (endsWith(image_path_visualize(), "png")) {
      rnd <- list(
        src = image_path_visualize(),
        contentType = "image/png"
      )

      shinyjs::show("monitorClimate_PNG")
      shinyjs::hide("monitorClimate_MP4")

      output$monitorClimate_PNG <- renderImage(rnd, deleteFile = FALSE)
    } else if (endsWith(image_path_visualize(), "mp4")) {
      rnd <- list(
        src = image_path_visualize(),
        contentType = "video/mp4"
      )

      shinyjs::show("monitorClimate_MP4")
      shinyjs::hide("monitorClimate_PNG")

      output$monitorClimate_MP4 <- renderUI({
        # Temporarily copy video to www directory
        # Reason: Giving absolute path is not allowed due to security issues
        if (!dir.exists(videoDir())) {
          dir.create(videoDir())
        }

        tmpVideo <- file.path(videoDir(), "animation.mp4")

        file.copy(
          from = image_path_visualize(),
          to = tmpVideo,
          overwrite = TRUE
        )

        # Render the video
        tags$video(
          id = "video",
          type = "video/mp4",
          src = "video/animation.mp4",
          controls = "controls")
      })
    }

    # Start timer independently of 1D/2D plot.
    shinyjs::delay(2000, readyToPlot(TRUE))

  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # Observe changes to lon, lat slider and update image plot width/height
  observeEvent({
    lon_bounds()
    lat_bounds()
  }, {

    req(visualizeVariables()$plot_dim == 2)

    imDim <- cmsafvis::recalculateImageDimensions(
      visualizeVariables = visualizeVariables(),
      lon_bounds = lon_bounds(),
      lat_bounds = lat_bounds(),
      image_def = image_def,
      ihsf = ihsf
      )

    imagewidth(imDim$imagewidth)
    imageheight(imDim$imageheight)
    #
    #
    # lon <- visualizeVariables()$lon[visualizeVariables()$lon <= lon_bounds()[2]]
    # lon <- lon[lon_bounds()[1] <= lon]
    #
    # lat <- visualizeVariables()$lat[visualizeVariables()$lat <= lat_bounds()[2]]
    # lat <- lat[lat_bounds()[1] <= lat]
    #
    # # Update this value if you want to change min width/height of plot.
    # minSize <- 200
    # tmpWidth  <- max(minSize, image_def)
    # tmpHeight <- max(minSize, image_def)
    #
    # # Update width and height according to visualizeVariables lat and lon vectors
    # if (length(lon) >= length(lat)) {
    #   # Shrink height
    #   tmpHeight <- round(tmpWidth * length(lat) / length(lon))
    #   if (tmpHeight < minSize) {
    #     tmpWidth <- minSize / tmpHeight * tmpWidth
    #     tmpHeight <- minSize
    #   }
    #
    #   # Why are we doing this? (And why not in the else block?)
    #   imageheight(tmpHeight + (round((ihsf * tmpHeight))))
    #   imagewidth(tmpWidth)
    # } else {
    #   # No need to check against minSize since we will multiply with a value > 1.
    #   tmpWidth <- round(tmpHeight * length(lat) / length(lon))
    #
    #   imagewidth(tmpWidth)
    #   imageheight(tmpHeight)
    # }

    lat_lon_trigger(lat_lon_trigger() + 1)
  })

  # Render scale ranges when data range changes.
  output$num_rmin <- renderUI({
    numericInput("num_rmin",
                 label = "Scale Range Min",
                 value = round(visualizeDataMin(), digits = 1),
                 step = 0.1)
  })

  # Render scale ranges when data range changes.
  output$num_rmax <- renderUI({
    if (round(visualizeDataMin(), digits = 1) == round(visualizeDataMax(), digits = 1)) {
      value <- round(visualizeDataMax(), digits = 1) + 0.1
    } else {
      value <- round(visualizeDataMax(), digits = 1)
    }

    numericInput("num_rmax",
                 label = "Scale Range Max",
                 value = value,
                 step = 0.1)
  })

  # Y-Range of 1D plot.
  output$y_visualize <- renderUI({
    req(visualizeDataMax())
    req(visualizeDataMin())

    diff <- visualizeDataMax() - visualizeDataMin()

    sliderInput("slidery",
                label = "Y-Range",
                min = round(visualizeDataMin() - (0.25 * diff), 1),
                max = round(visualizeDataMax() + (0.25 * diff), 1),
                value = c(trunc(visualizeDataMin(), 1), trunc(visualizeDataMax())))
  })

  # Observe changes to visualize data. If all data are available update min and max values globally.
  # Else we'll need to keep track of them
  observe({
    req(class(visualizeVariables()$data) == "try-error")

    min_data <- min(visualizeDataTimestep(), na.rm = TRUE)
    max_data <- max(visualizeDataTimestep(), na.rm = TRUE)

    if (round(min_data, digits = 1) == round(max_data, digits = 1)) {
      min_data <- min_data - 0.05
      max_data <- max_data + 0.05
    }

    visualizeDataMin(min_data)
    visualizeDataMax(max_data)
  })

  # Observing changes to shape file path
  observeEvent(shapeFile_path_action(), {
    region_data(countriesHigh)
    req(shapeFile_path())
    req(file.exists(shapeFile_path()))

    # data of all regions
    region_data(suppressWarnings(maptools::readShapePoly(shapeFile_path())))
  })

  # Set divisions
  observeEvent(region_data(), {
    all_divisions <- names(region_data())
    if (!is.element("COUNTRY", all_divisions)) {
      all_divisions <- c("COUNTRY", all_divisions)
    }
    output$division_options <- renderUI({
      selectInput("division",
                  "Division",
                  choices = c("Select division", all_divisions))
    })
  })

  # Set regions
  observeEvent(input$division, {
    if (input$division != "Select division") {
      if (input$division != "COUNTRY") {
        all_regions <- levels(region_data()[[input$division]])
      } else {
        # data of all countries
        countries_choosable <- codes[, "iso3c"]
        names(countries_choosable) <- codes[, "country.name.en"]

        all_regions <- countries_choosable
      }

      output$region_options <- renderUI({
        selectInput("region",
                    "Region",
                    choices = c("Select region", all_regions),
                    selected = "Select region")
      })
    } else {
      output$region_options <- renderUI({
        selectInput("region",
                    "Region",
                    choices = c("Select region"))
      })
    }
  })

  # Toggle instat file upload
  observeEvent(input$plot_rinstat, {
    # Is set in global.R
    if (isRunningLocally) {
      if (input$plot_rinstat) {
        shinyjs::show("instat_file_local")
      } else {
        shinyjs::hide("instat_file_local")
      }
    } else {
      if (input$plot_rinstat) {
        shinyjs::show("instat_file_remote")
      } else {
        shinyjs::hide("instat_file_remote")
      }
    }
  }, ignoreInit = TRUE)

  # Go back to set up page.
  observeEvent(input$backToSetup, {
    resetToVisualizePanel()
  }, ignoreInit = TRUE)

  # Go back to set up page (from monitor climate visualization).
  observeEvent(input$backToSetup2, {
    # Unlink viedo dir
    if (dir.exists(videoDir())) {
      unlink(
        x = videoDir(),
        recursive = TRUE,
        force = TRUE
      )
    }

    resetToVisualizePanel()
  }, ignoreInit = TRUE)

  # Reacting to instat file data.
  instat.data <- reactive({
    req(instat_path())
    req(endsWith(instat_path(), ".RData"))

    a <- get(load(instat_path()))
    a <- as.data.frame(a)
    a
  })

  # Combine the CM SAF R Toolbox and R-Instat
  co.data <- reactive({
    # Only recompute if we want to add r_instat stuff to plot.
    req(instat_path())
    req(endsWith(instat_path(), ".RData"))
    # check row.names of data frame
    lo_dummy <- c("lon", "longitude", "laenge", "x")
    la_dummy <- c("lat", "latitude", "breite", "y")
    ti_dummy <- c("time", "date", "zeit", "t")
    da_dummy <- c("data", "daten", "z", "element")

    dn <- attr(instat.data(), "element_name")
    if (!is.null(dn)) {
      da_dummy <- append(da_dummy, dn)
    } else {
      dn <- attr(instat.data(), "data_name")
      if (!is.null(dn)) {
        da_dummy <- append(da_dummy, dn)
      }
    }

    instat_names <- names(instat.data())

    lo_n <- 0
    la_n <- 0
    ti_n <- 0
    da_n <- 0

    for (i in seq_along(instat_names)) {
      if (toupper(instat_names[i]) %in% toupper(lo_dummy)) (lo_n <- i)
      if (toupper(instat_names[i]) %in% toupper(la_dummy)) (la_n <- i)
      if (toupper(instat_names[i]) %in% toupper(ti_dummy)) (ti_n <- i)
      if (toupper(instat_names[i]) %in% toupper(da_dummy)) (da_n <- i)
    }

    if (lo_n > 0 & la_n > 0 & ti_n > 0 & da_n > 0) {

      # check monthly or daily
      # station
      time_station <- instat.data()[, ti_n]
      if (length(time_station) > 500) (time_station <- time_station[1:500])
      mon_station  <- format(as.Date(time_station), "%m")
      year_station <- format(as.Date(time_station), "%Y")
      day_station  <- format(as.Date(time_station), "%d")
      dummy <- which(mon_station == mon_station[1] & year_station == year_station[1])
      mmdm <- "d"
      if (length(unique(day_station[dummy])) == 1) {
        mmdm <- "m"
      }

      # satellite
      time_sat <- visualizeVariables()$date.time
      if (length(time_sat) > 40) (time_sat <- time_sat[1:40])
      mon_sat  <- format(as.Date(time_sat), "%m")
      year_sat <- format(as.Date(time_sat), "%Y")
      day_sat  <- format(as.Date(time_sat), "%d")
      dummy <- which(mon_sat == mon_sat[1] & year_sat == year_sat[1])
      mmdm_sat <- "d"
      if (length(unique(day_sat[dummy])) == 1) {
        mmdm_sat <- "m"
      }

      # extract data for chosen time step
      if (mmdm == "m" & mmdm_sat == "m") {
        match_time   <- which(format(as.Date(instat.data()[, ti_n]), "%Y-%m") == format(as.Date(input$timestep), "%Y-%m"), arr.ind = TRUE)
      } else {
        match_time   <- which(instat.data()[, ti_n] == input$timestep, arr.ind = TRUE)
      }

      lon_station  <- instat.data()[, lo_n][match_time]
      lat_station  <- instat.data()[, la_n][match_time]
      data_station <- instat.data()[, da_n][match_time]

      # delete NAs
      dummy <- !is.na(data_station)
      data_station <- data_station[dummy]
      data_station <- data_station
      lon_station  <- lon_station[dummy]
      lat_station  <- lat_station[dummy]
      # Extract corresponding data points
      data_sat <- c(seq_along(data_station))

      for (istation in seq_along(data_station)) {
        # find closest point to target coordinates using sp package
        lon <- input$slider1
        lat <- input$slider2

        dlon <- abs(lon[1] - lon[2])
        dlat <- abs(lat[1] - lat[2])

        lon_limit <- which(lon >= (lon_station[istation] - dlon) & lon <= (lon_station[istation] + dlon))
        lat_limit <- which(lat >= (lat_station[istation] - dlat) & lat <= (lat_station[istation] + dlat))

        if (any(lon_limit) & any(lat_limit)) {

          lon2 <- lon[lon_limit]
          lat2 <- lat[lat_limit]

          pos <- sp::SpatialPoints(cbind(lon_station[istation], lat_station[istation]), proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
          dum_dist <- 1000
          for (i in seq_along(lon2)) {
            for (j in seq_along(lat2)) {
              dist <- sp::spDistsN1(pos, c(lon2[i], lat2[j]), longlat = FALSE)
              if (dist <= dum_dist) {
                dum_dist <- dist
                dumi <- i
                dumj <- j
              }
            }
          }

          lon_limit <- which(lon == lon2[dumi])
          lat_limit <- which(lat == lat2[dumj])
        }

        if (any(lon_limit) & any(lat_limit)) {
          data_sat[istation] <- visualizeDataTimestep()[lon_limit, lat_limit]
          # data_sat[istation] <- visualizeVariables()$data[lon_limit,lat_limit,which(visualizeVariables()$date.time == input$timestep,arr.ind = TRUE)]
        }
      }

      cd <- data.frame(data_station, data_sat, lon_station, lat_station)
      cd
    } # end if lo_n,la_n,ti_n,da_n
  })

  # A function to validate all numeric inputs.
  # This should only be called knowing all required inputs exist!
  validNumericInputs <- reactive({
    validity <- list()
    # Set to false first
    validity$valid <- FALSE

    # Occurs either way
    if (!is.numeric(input$num_brk) || input$num_brk < 2 || input$num_brk > 64) {
      validity$argument <- "Number of colors"
      validity$value <- input$num_brk
      validity$required <- "in [2, 64]"
      return(validity)
    }

    if (input$slider1[1] == input$slider1[2]) {
      validity$argument <- "Longitude slider"
      validity$value <- "min = max"
      validity$required <- "min < max"
      return(validity)
    }

    if (input$slider2[1] == input$slider2[2]) {
      validity$argument <- "Latitude slider"
      validity$value <- "min = max"
      validity$required <- "min < max"
      return(validity)
    }

    if (input$proj == "rect") {
      # inputs only in rectangular

      if (!is.numeric(input$num_tick) || input$num_tick < 2 || input$num_tick > 64) {
        validity$argument <- "Number of ticks"
        validity$value <- input$num_tick
        validity$required <- "in [2, 64]"
        return(validity)
      }

      if (!is.numeric(input$num_rmin) || !is.numeric(input$num_rmax) || input$num_rmin >= input$num_rmax) {
        if (is.na(input$num_rmin) && is.na(input$num_rmax)) {
          validity$argument <- "Scale range"
          validity$value <- "Seems like you are trying to visualize data that consits of only 'NA' values. Please choose another file."
          validity$required <- "min < max"

        } else {
          validity$argument <- "Scale range"
          validity$value <- paste("min =", input$num_rmin, "max =", input$num_rmax)
          validity$required <- "min < max"

        }
        return(validity)
      }

      if (input$location) {
        if (!is.numeric(input$lon_loc) || abs(input$lon_loc) > 180) {
          validity$argument <- "Location"
          validity$value <- input$lon_loc
          validity$required <- "in [-180, 180]"
          return(validity)
        }

        if (!is.numeric(input$lat_loc) || abs(input$lat_loc) > 90) {
          validity$argument <- "Location"
          validity$value <- input$lat_loc
          validity$required <- "in [-180, 180]"
          return(validity)
        }
      }
    } else if (input$proj == "ortho") {
      # inputs only in orthoprojection
      if (!is.numeric(input$yort) || abs(input$yort) > 180) {
        validity$argument <- "Center Lon"
        validity$value <- input$yort
        validity$required <- "in [-180, 180]"
        return(validity)
      }

      if (!is.numeric(input$xort) || abs(input$xort) >= 90) {
        validity$argument <- "Center Lat"
        validity$value <- input$xort
        validity$required <- "in (-90, 90)"
        return(validity)
      }

      if (!is.numeric(input$rort) || abs(input$rort) >= 90) {
        validity$argument <- "Rotation"
        validity$value <- input$rort
        validity$required <- "in (-90, 90)"
        return(validity)
      }
    }

    # if not returned false yet, return true.
    validity$valid <- TRUE
    return(validity)
  })

  observeEvent(input$add_loc, {
    req(input$add_loc)
    req(abs(input$lon_loc) <= 180)
    req(abs(input$lat_loc) <= 90)
    if (!is.element(input$lon_loc, lon_loc_vec()) || !is.element(input$lat_loc, lat_loc_vec())) {
      lon_loc_vec(append(lon_loc_vec(), input$lon_loc))
      lat_loc_vec(append(lat_loc_vec(), input$lat_loc))
      name_loc_vec(append(name_loc_vec(), input$name_loc))
    }
  }, ignoreInit = TRUE)

  # Get new data if timestep is updated
  observeEvent(input$timestep, {
    req(is.element(input$timestep, visualizeVariables()$date.time))
    # Update the data according to the time step using the visualize nc file's id and previously calculated variable.
    id <- ncdf4::nc_open(nc_path_visualize())
    tmp <- getVariableData(which(visualizeVariables()$date.time == input$timestep), id, visualizeVariables()$vn)
    ncdf4::nc_close(id)

    if (reversedDimensions$transpose) {
      tmp <- aperm(tmp, c(2, 1))
    }

    if (reversedDimensions$lonReverse) {
      tmp <- tmp[rev(seq_len(length(visualizeVariables()$lon))), ]
    }

    if (reversedDimensions$latReverse) {
      tmp <- tmp[, rev(seq_len(length(visualizeVariables()$lat)))]
    }
    visualizeDataTimestep(tmp)
  })

  ## Colorpalette Stuff ##
  palettes <- GetPaletteConfig(gui = TRUE)

  # ----------------------------------------------------------------
  # Getting currently selected color scheme
  # ----------------------------------------------------------------
  palettes <- GetPaletteConfig(gui = TRUE)
  names(palettes) <- tolower(names(palettes))
  names(palettes)[names(palettes) == "typ"] <- "type"

  # add more color schemes
  new_row <- data.frame("more", NA, NA, NA, NA, NA, NA, NA, NA, NA, 1)
  names(new_row) <- names(palettes)
  palettes <- rbind(palettes, new_row)
  rownames(palettes)[75] <- "tim.colors"

  palettes <- rbind(palettes, new_row)
  rownames(palettes)[76] <- "sunny"

  x <- list()
  for (i in seq_len(nrow(palettes))) {
    x[[sprintf("%s", rownames(palettes)[i])]] <- rownames(palettes)[i]
  }
  updateSelectInput(session, "PAL", choices = x, selected = "sunny")

  # Debouncing
  db_xort  <- shiny::debounce(reactive({input$xort}),  750)
  db_yort  <- shiny::debounce(reactive({input$yort}),  750)
  db_rort  <- shiny::debounce(reactive({input$rort}),  750)
  db_num_tick <- shiny::debounce(reactive({input$num_tick}), 750)
  db_num_rmin <- shiny::debounce(reactive({input$num_rmin}), 750)
  db_num_rmax <- shiny::debounce(reactive({input$num_rmax}), 750)
  db_num_brk  <- shiny::debounce(reactive({input$num_brk}),  750)
  db_lat_lon_trigger <- shiny::debounce(reactive({lat_lon_trigger()}), 750)
  db_int        <- shiny::debounce(reactive({input$int}),    750)
  db_region     <- shiny::debounce(reactive({input$region}), 750)
  db_checkGroup <- shiny::debounce(reactive({input$checkGroup}), 750)
  db_visualizeDataTimestep <- shiny::debounce(reactive({visualizeDataTimestep()}), 750)
  db_visualizeDataMax <- shiny::debounce(reactive({visualizeDataMax()}), 750)
  db_proj <- shiny::debounce(reactive({input$proj}), 750)
  db_text2 <- shiny::debounce(reactive({input$text2}), 750)
  db_text3 <- shiny::debounce(reactive({input$text3}), 750)
  db_text1 <- shiny::debounce(reactive({input$text1}), 1000)

  getPlot_1d <- reactive({
    req(readyToPlot())

    # Triggers and requirements
    req(is.character(db_text1_1d()))
    req(db_sliderx())
    req(db_slidery())
    req(db_integer())
    req(db_checkGroup_type())
    c(db_trend())
    c(db_analyze_timeseries())
    req(db_ticknumber())
    req(db_dateformat())
    c(db_text2_1d())

    # Catch data is error exception
    if (class(visualizeVariables()$data) == "try-error") {
      showModal(modalDialog(
        h4("We can't handle your file at the moment. Please try another file."),
        br(),
        title = "Sorry!",
        size = "l"
      ))

      # Silently leave.
      req(FALSE)
    }

    isolate({
      res_plot <- try(cmsafvis::render_plot_1d(fileExtension = ".png",
                                               visualizeVariables = visualizeVariables(),
                                               ticknumber = input$ticknumber,
                                               dateformat = input$dateformat,
                                               analyze_timeseries = input$analyze_timeseries,
                                               addTrend = input$trend,
                                               sliderx = input$sliderx,
                                               slidery = input$slidery,
                                               checkGroup_type = input$checkGroup_type,
                                               imagewidth = imagewidth(),
                                               imageheight = imageheight(),
                                               text1_1d = input$text1_1d,
                                               text2_1d = input$text2_1d,
                                               textsize = textsize,
                                               linesize = linesize,
                                               col = input$integer))
    })

    if (class(res_plot) != "try-error") {
      return(res_plot)
    } else {
      showModal(modalDialog(
        br(),
        h3("Something went wrong while creatin 1D Plot."),
        title = "Error.",
        size = "l"
      ))
      req(NULL)
    }
  })

  # A reactive, throttled function for generating the plot.
  getPlot_2d <- reactive({
    req(readyToPlot())

    # Required triggers
    req(nrow(db_visualizeDataTimestep()) > 0)  # when data at timestep changes
    req(nchar(db_text1()) > 0)                        # Need a title
    req(db_proj())                         # projection
    req(db_visualizeDataMax())             # max data (not sure why want to trigger this?)

    # Isolated requirements
    isolate(req(lon_bounds()))    # Require this to prevent error message
    isolate(req(lat_bounds()))    # However, do not trigger on change
    # isolate( req(imagewidth()) )    # image width (triggering is done by lat_lon_trigger)
    # isolate( req(imageheight()) )   # image height (triggering is done by lat_lon_trigger)

    # Non-required triggers (some are required but will be caught in vilidation of numeric inputs.)
    c(name_loc_vec(),    # new location
      db_checkGroup(),  # colorbar
      db_text2(),        # sub-title
      db_text3(),        # scale caption
      db_int(),         # country borders
      db_region(),      # for region plot
      db_xort(),         # center lat
      db_yort(),         # center lon
      db_rort(),         # rotation
      instat_path(),     # instat file path change
      db_num_tick(),     # number ticks
      db_num_rmin(),         # min val
      db_num_rmax(),         # max val
      db_num_brk(),          # number breaks
      db_lat_lon_trigger(),  # changes to lat, lon, bounds
      input$reverse,         # revert colors
      input$PAL              # colorspace pallete
    )

    # Everything below this point is non-reactive.
    isolate({
      # First check validity in region plot
      if (input$plot_region) {
        if (is.null(region_data()) && (is.null(shapeFile_path()) || !file.exists(shapeFile_path()))) {
          # show message
          shinyjs::hide("spinner_plot1")
          shinyjs::enable("backToSetup")

          showModal(modalDialog(
            br(),
            h3("Please select a shape file ",
               tags$strong("(.shp)"),
               " or select the COUNTRY division to continue."),
            title = "Wrong file format.",
            size = "l"
          ))

          req(FALSE)
        }
        req(input$region != "Select region")
      }

      # Show spinner
      shinyjs::show("spinner_plot1", anim = TRUE, animType = "fade")
      shinyjs::disable("backToSetup")

      # Toggle state of plot_rinstat.
      plot_rinstat <- FALSE
      if (input$plot_rinstat) {
        if (!is.null(instat_path()) && file.exists(instat_path())) {
          plot_rinstat <- TRUE
        } else {
          # show message
          shinyjs::hide("spinner_plot1")
          shinyjs::enable("backToSetup")

          showModal(modalDialog(
            br(),
            h3("Please select a .RData instat file to continue."),
            title = "Wrong file format.",
            size = "l"
          ))

          # Leave silently.
          req(FALSE)
        }
      }

      # Validate numeric inputs
      validity <- validNumericInputs()
      if (validity$valid != TRUE) {
        # show message
        shinyjs::hide("spinner_plot1")
        shinyjs::enable("backToSetup")

        showModal(modalDialog(
          br(),
          h5(paste0("Argument: ", validity$argument)),
          h5(paste0("Value: ", validity$value)),
          h5(paste0("Expected: ", validity$required)),
          title = "Wrong input argument.",
          size = "l"
        ))

        # Leave silently.
        req(FALSE)
      }

      if (input$plot_region) {
        res <- try(ls <- cmsafvis::render_region_plot(infile = isolate(nc_path_visualize()),
                                                      visualizeVariables = visualizeVariables(),
                                                      visualizeDataMax = visualizeDataMax(),
                                                      lon_bounds = lon_bounds(),
                                                      lat_bounds = lat_bounds(),
                                                      lon_loc_vec = lon_loc_vec(),
                                                      lat_loc_vec = lat_loc_vec(),
                                                      name_loc_vec = name_loc_vec(),
                                                      division = input$division,
                                                      selectedRegion = input$region,
                                                      region_data = region_data(),
                                                      timestep = input$timestep,
                                                      num_tick = input$num_tick,
                                                      num_rmin = input$num_rmin,
                                                      num_rmax = input$num_rmax,
                                                      location = input$location,
                                                      text1 = input$text1,
                                                      text2 = input$text2,
                                                      text3 = input$text3,
                                                      PAL = input$PAL,
                                                      palettes = palettes,
                                                      num_brk = input$num_brk,
                                                      reverse = input$reverse,
                                                      textsize = textsize,
                                                      bordercolor = bordercolor,
                                                      plot_grid = plot_grid,
                                                      grid_col = grid_col,
                                                      image_def = image_def,
                                                      ihsf = ihsf))
      } else {
        res <- try(ls <- cmsafvis::render_plot(plot_rinstat = input$plot_rinstat,
                                               visualizeVariables = visualizeVariables(),
                                               visualizeDataTimestep = visualizeDataTimestep(),
                                               nc_path_visualize = nc_path_visualize(),
                                               visualizeDataMax = visualizeDataMax(),
                                               lon_bounds = lon_bounds(),
                                               lat_bounds = lat_bounds(),
                                               lon_loc_vec = lon_loc_vec(),
                                               lat_loc_vec = lat_loc_vec(),
                                               name_loc_vec = name_loc_vec(),
                                               timestep = input$timestep,
                                               num_tick = input$num_tick,
                                               num_rmin = input$num_rmin,
                                               num_rmax = input$num_rmax,
                                               num_brk = input$num_brk,
                                               co.data = co.data(),
                                               proj = input$proj,
                                               imagewidth = imagewidth(),
                                               imageheight = imageheight(),
                                               xort = input$xort,
                                               yort = input$yort,
                                               rort = input$rort,
                                               slider1 = input$slider1,
                                               slider2 = input$slider2,
                                               location = input$location,
                                               text1 = input$text1,
                                               text2 = input$text2,
                                               text3 = input$text3,
                                               int = input$int,
                                               textsize = textsize,
                                               bordercolor = bordercolor,
                                               linesize = linesize,
                                               na.color = na.color,
                                               PAL = input$PAL,
                                               palettes = palettes,
                                               reverse = input$reverse,
                                               plot_grid = plot_grid,
                                               grid_col = grid_col))
      }
    })
    if (class(res) == "try-error") {
      showModal(modalDialog(
        h4("An error occured while creating the plot. Please try another file or other input arguments."),
        tags$p(paste0("Message: ", res)),
        title = "Error!",
        size = "m"
      ))
      shinyjs::enable("backToSetup")
      shinyjs::hide("spinner_plot1")

      # Leave silently
      req(FALSE)
    } else {
      # Updating path to png plot
      png_path(ls$src)

      # show plot
      shinyjs::enable("backToSetup")
      shinyjs::hide("spinner_plot1")
      shinyjs::show("myImage", anim = TRUE, animType = "fade")

      # Return the image and don't delete it for now. It is stored in tmp directory anyway so it will be gone after the session.
      # We want to keep it so we can downlaod it later.
      ls
    }
  })

  # Rendering image 2d
  output$myImage_2d <- renderImage({
    getPlot_2d()
  }, deleteFile = FALSE)

  # Debounce 1D plot
  db_sliderx    <- shiny::debounce(reactive({input$sliderx}), 750)
  db_slidery    <- shiny::debounce(reactive({input$slidery}), 750)
  db_integer    <- shiny::debounce(reactive({input$integer}), 750)
  db_trend      <- shiny::debounce(reactive({input$trend}), 750)
  db_checkGroup_type    <- shiny::debounce(reactive({input$checkGroup_type}), 750)
  db_analyze_timeseries <- shiny::debounce(reactive({input$analyze_timeseries}), 750)
  db_ticknumber <- shiny::debounce(reactive({input$ticknumber}), 750)
  db_dateformat <- shiny::debounce(reactive({input$dateformat}), 750)
  db_text2_1d   <- shiny::debounce(reactive({input$text2_1d}), 750)

  # Again, this debounce is special. Compare with special debounce of plot 2d
  db_text1_1d   <- shiny::debounce(reactive({input$text1_1d}), 1000)

  # Copied from app-4
  output$myImage_1d <- renderImage({
    ls <- getPlot_1d()
    png_path(ls$src)
    ls
  },deleteFile = FALSE)

  # Creating a preview plot.
  output$previewSpatialCoveragePlot_vis <- renderPlot({
    req(input$slider1)
    req(input$slider2)
    maps::map("world", fill = TRUE, col = "gray36", bg = "white", xlim = input$slider1, ylim = input$slider2)
    maps::map("world", fill = TRUE, col = "gray36", bg = "white", xlim = input$slider1, ylim = input$slider2)
    title(main = "Zoom by selecting a region on this plot.")
  })

  # Observing changes in brushing
  observe({
    if (is.null(input$zoom_brush)) {
      lon_bounds(input$slider1)
      lat_bounds(input$slider2)
    } else {
      brush <- input$zoom_brush
      lon <- c(brush$xmin, brush$xmax)
      lat <- c(brush$ymin, brush$ymax)
      lon_bounds(lon)
      lat_bounds(lat)
    }
  })

  # Download data for 1D-plot.
  output$downloadFile <- downloadHandler(
    filename = function() {
      return(paste0("data_", input$text1_1d, sessionName, ".csv"))
    },
    content = function(file) {
      # TODO: NOT SURE IF THIS WILL WORK IN REMOTE HOST. PLEASE TRY ON SHADOW!!
      removeModal()
      sep <- switch(as.numeric(input$separator), ";", ",", "\t")
      dataframe <- data.frame(visualizeVariables()$date.time, visualizeVariables()$data[,,])
      names(dataframe) <- c("dateTime", "data")
      utils::write.table(dataframe, file, row.names = FALSE, sep = sep)
    },
    contentType = "text/comma-separated-values"
  )

  output$downloadPlot <- downloadHandler(
    #filename:
    filename = function() {
      ext <- switch(as.numeric(input$imageformat), ".png", ".kml", ".tif", ".jpg", ".pdf")
      selected <- input$imageformat
      if (visualizeVariables()$plot_dim == 2) {
        return(paste0("plot_", input$text1, sessionName, ext))
      } else {
        return(paste0("plot_", input$text1_1d, sessionName, ext))
      }
    },
    content = function(file) {
      # TODO: NOT SURE IF THIS WILL WORK IN REMOTE HOST. PLEASE TRY ON SHADOW!!
      withProgress(message = "downloading File", value = 0,
                   {
                     removeModal()
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.25)
                     }
                     if (input$imageformat == 1) {
                       file.copy(png_path(), file)
                     } else {
                       fileExtension <- switch(as.numeric(input$imageformat), ".png", ".kml", ".tif", ".jpg", ".pdf")

                       if (input$plot_region) {
                         res_plot <- cmsafvis::render_region_plot(infile = nc_path_visualize(),
                                                                  fileExtension = fileExtension,
                                                                  visualizeVariables = visualizeVariables(),
                                                                  visualizeDataMax = visualizeDataMax(),
                                                                  lon_bounds = lon_bounds(),
                                                                  lat_bounds = lat_bounds(),
                                                                  lon_loc_vec = lon_loc_vec(),
                                                                  lat_loc_vec = lat_loc_vec(),
                                                                  name_loc_vec = name_loc_vec(),
                                                                  division = input$division,
                                                                  selectedRegion = input$region,
                                                                  region_data = region_data(),
                                                                  timestep = input$timestep,
                                                                  num_tick = input$num_tick,
                                                                  num_rmin = input$num_rmin,
                                                                  num_rmax = input$num_rmax,
                                                                  location = input$location,
                                                                  text1 = input$text1,
                                                                  text2 = input$text2,
                                                                  text3 = input$text3,
                                                                  PAL = input$PAL,
                                                                  palettes = palettes,
                                                                  num_brk = input$num_brk,
                                                                  reverse = input$reverse,
                                                                  textsize = textsize,
                                                                  bordercolor = bordercolor,
                                                                  plot_grid = plot_grid,
                                                                  grid_col = grid_col,
                                                                  image_def = image_def,
                                                                  ihsf = ihsf)
                         in_plot <- res_plot$src
                       } else {
                         res_plot <- cmsafvis::render_plot(plot_rinstat = input$plot_rinstat,
                                                           fileExtension = fileExtension,
                                                           visualizeVariables = visualizeVariables(),
                                                           visualizeDataTimestep = visualizeDataTimestep(),
                                                           nc_path_visualize = nc_path_visualize(),
                                                           visualizeDataMax = visualizeDataMax(),
                                                           lon_bounds = lon_bounds(),
                                                           lat_bounds = lat_bounds(),
                                                           lon_loc_vec = lon_loc_vec(),
                                                           lat_loc_vec = lat_loc_vec(),
                                                           name_loc_vec = name_loc_vec(),
                                                           timestep = input$timestep,
                                                           num_tick = input$num_tick,
                                                           num_rmin = input$num_rmin,
                                                           num_rmax = input$num_rmax,
                                                           num_brk = input$num_brk,
                                                           co.data = co.data(),
                                                           proj = input$proj,
                                                           imagewidth = imagewidth(),
                                                           imageheight = imageheight(),
                                                           xort = input$xort,
                                                           yort = input$yort,
                                                           rort = input$rort,
                                                           slider1 = input$slider1,
                                                           slider2 = input$slider2,
                                                           location = input$location,
                                                           text1 = input$text1,
                                                           text2 = input$text2,
                                                           text3 = input$text3,
                                                           int = input$int,
                                                           textsize = textsize,
                                                           bordercolor = bordercolor,
                                                           linesize = linesize,
                                                           na.color = na.color,
                                                           PAL = input$PAL,
                                                           palettes = palettes,
                                                           reverse = input$reverse,
                                                           plot_grid = plot_grid,
                                                           grid_col = grid_col)
                         in_plot <- res_plot$src
                       }
                       file.copy(in_plot,file)
                     }
                   })
    },
    contentType = switch(as.numeric(input$imageformat), "image/png", "image/kml", "image/tif", "image/jpg", "image/pdf")
  )

  # Statsics output
  observe({
    # Requirements
    req(input$lonPoint)
    req(input$latPoint)
    req(input$timestep)
    req(visualizeVariables()$date.time)
    req(visualizeDataMax())
    req(visualizeVariables()$plot_dim == 2)

    # compute some statistics
    xr <- which(visualizeVariables()$lon >= (input$slider1[1]) & visualizeVariables()$lon <= (input$slider1[2]))
    yr <- which(visualizeVariables()$lat >= (input$slider2[1]) & visualizeVariables()$lat <= (input$slider2[2]))
    dastat <- visualizeDataTimestep()[xr, yr]
    # dastat <- visualizeVariables()$data[xr, yr, which(visualizeVariables()$date.time == input$timestep, arr.ind = TRUE)]

    dg <- 2
    if (abs(visualizeDataMax()) >= 10) (dg <- 1)
    if (abs(visualizeDataMax()) >= 100) (dg <- 0)

    da_mean   <- round(mean(dastat, na.rm = TRUE), digits = dg)
    da_median <- round(stats::median(dastat, na.rm = TRUE), digits = dg)
    da_sd     <- round(stats::sd(dastat, na.rm = TRUE), digits = dg)
    da_max    <- round(max(dastat, na.rm = TRUE), digits = dg)
    da_min    <- round(min(dastat, na.rm = TRUE), digits = dg)

    # some numbers
    output$statistics <- renderPrint({
      cat(paste0("Mean:               ", da_mean), "\n")
      cat(paste0("Median:             ", da_median), "\n")
      cat(paste0("Standard deviation: ", da_sd), "\n")
      cat(paste0("Maximum:            ", da_max), "\n")
      cat(paste0("Minimum:            ", da_min), "\n")
      cat(paste0("Unit:               ", visualizeVariables()$unit), "\n")
    })

    # Missing values can be found in global.R
    # histogram
    output$myHist <- renderPlot({
      cmsafvis::render_hist_plot(dastat = as.numeric(dastat),
                                 shortDescription = input$text1,
                                 xlab = input$text3,
                                 grid_col = grid_col,
                                 bordercolor = bordercolor,
                                 linesize = linesize)
    })
  })

  # Observing changes to instat file
  observeEvent({
    instat_path_action()
    input$plot_rinstat
  }, {
    # Requirements
    req(instat_path())
    if (input$plot_rinstat && file.exists(instat_path())) {
      # grid_col, bordercolor, and linesize can be found in global.R
      output$myComp <- renderPlot({
        cmsafvis::render_instat_plot(co.data = co.data(),
                                     shortDescription = input$text1,
                                     ylab = input$text3,
                                     grid_col = grid_col,
                                     bordercolor = bordercolor,
                                     linesize = linesize)
      })
    }
  })

  # File summaries
  output$summary1 <- renderPrint({
    req(nc_path_visualize())
    cmsafops::ncinfo(nc_path_visualize())
  })

  output$summary2 <- renderPrint({
    req(nc_path_visualize())
    cmsafops::ncinfo(nc_path_visualize(), "m")
  })

  # About part
  output$about <- renderPrint({
    cat("The CMSAF Visualizer is part of the CM SAF R Toolbox.", "\n")
    cat("This tool helps you to visualize 1D-timeseries and 2D-maps.", "\n")
    cat("\n")
    cat("This version ('Share and Enjoy') was tested with the cmsaf", "\n")
    cat("R-package in version 3.0.0.", "\n")
    cat("\n")
    cat("Suggestions for improvements and praise for the developers", "\n")
    cat("can be send to contact.cmsaf@dwd.de.", "\n")
    cat("\n")
    cat("                              - Steffen Kothe - 2020-07-24 -", "\n")
    cat("\n")
    cat("\n")
  })

  # Tipps
  output$tipps <- renderPrint({
    cat("You can easily plot station data, which were exported by R-Instat.", "\n")
    cat("\n")
    cat("If saving an image fails, try right-click plus 'save image as'.", "\n")
    cat("\n")
    cat("The orthographic projection includes some interpolation,", "\n")
    cat("which can have an impact on local pixel values!", "\n")
    cat("\n")
    cat("If you swim with a friend, your chances of getting eaten", "\n")
    cat("by a shark will drop by 50%.", "\n")
    cat("\n")
  })

  output$link <- renderUI({
    url <- a("http://www.cmsaf.eu/R_toolbox", href = "http://www.cmsaf.eu/R_toolbox")
    tagList("URL link:", url)
  })

  #### Destructors ####
  # Stop app on exit button.
  observeEvent(input$exit, {
    stopApp(returnValue = invisible(99))
  })

  # After app is closed do cleanup. (Only if directory hasn't existed before session.)
  session$onSessionEnded(function() {
    # Variable 'isRunningLocally' can be found in global.R
    # Only removing in remote session. Keeping all files in local versions.
    if (!isRunningLocally) {
      if (dir.exists(userDir)) {
        unlink(userDir, recursive = TRUE)
      }
    } else {
      # If no files contained remove user directory. (local host)
      if (length(list.files(userDir, recursive = TRUE, include.dirs = TRUE)) == 0) {
        unlink(userDir, recursive = TRUE)
      }
    }
  })


  downloadModal1d <- function(failed = FALSE) {
    modalDialog(column(6,
                       radioButtons("separator",
                                    label = "Choose separator",
                                    choices = list("Semicolon" = 1,
                                                   "Comma" = 2,
                                                   "Tab" = 3),
                                    selected = 1),
                       downloadButton("downloadFile", "Download Data")
    ),
    column(6,
           selectInput("imageformat",
                       label = "File format",
                       choices = list("PNG" = 1,
                                      "jpeg" = 4,
                                      "pdf" = 5
                       ),
                       selected = 1),
           downloadButton("downloadPlot", "Download Image")
    ),
    footer = tagList(
      modalButton("Cancel")
    )

    )
  }
  downloadModal2d <- function(failed = FALSE) {
    modalDialog(
      selectInput("imageformat",
                  label = "File format",
                  if (input$proj == "ortho") {
                    choices <- list("PNG" = 1,
                                   "jpeg" = 4,
                                   "pdf" = 5
                    )
                  } else {
                    choices <- list("PNG" = 1,
                                   "KML" = 2,
                                   "GeoTiff" = 3,
                                   "jpeg" = 4,
                                   "pdf" = 5
                    )
                  },
                  selected = 1),
      downloadButton("downloadPlot", "download Image"),
      footer = tagList(
        modalButton("Cancel")
      )

    )
  }

  # Downloader for monitor_climate
  output$download_monitor_climate <- renderUI({
    downloadButton(
      outputId = "download_monitor_climate_output",
      label = basename(image_path_visualize()),
      style = "width:100%;")
  })

  output$download_monitor_climate_output <- downloadHandler(
    filename = function() {
      paste0(basename(image_path_visualize()))
    },
    content = function(con) {
      file.copy(image_path_visualize(), con)
    }
  )

  # Show modal when button is clicked.
  observeEvent(input$showModal, {
    if (visualizeVariables()$plot_dim == 1) {
      showModal(downloadModal1d())
    } else {
      showModal(downloadModal2d())
    }
  })
}
