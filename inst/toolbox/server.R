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

  flist <- list.files(ordpath,substr(ordname,1,8), full.names = TRUE)

  tarlist <- NULL
  for (file in flist) {
    if (tar_flag == 1) {
      tarlist <- append(tarlist,utils::untar(file, list = TRUE, tar = "internal"))
    } else if (tar_flag == 2) {
      tarlist <- append(tarlist,utils::untar(file, list = TRUE, tar = Sys.getenv("TAR")))
    } else if (tar_flag == 3) {
      tarlist <- append(tarlist,utils::untar(file, list = TRUE, tar = "C:/Rtools/bin/tar.exe"))
    } else {
      tarlist <- append(tarlist,utils::untar(file, list = TRUE))
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
  return( "CM_SAF_CLAAS2_L2_AUX.nc" %in% tarlist )
}

# A function to extract ALL dates from a tar file.
extractAllDates <- function(path_to_tar, tar_flag) {
  # List the to be untared  file.
  tarlist <- getTarList(path_to_tar, tar_flag = tar_flag)

  # Timestep HAS TO BE RELEVANT FOR SOMETHING! FIND THAT OUT!!
  timestep <- substr(tarlist[1], 4, 4)

  allDates <- as.character(substr(tarlist, 6, 13))
  return( list(allDates = as.Date(allDates, "%Y%m%d"), timestep = timestep) )
}

# A function to extract start and end dates.
extractDateRange <- function(path_to_tar, tar_flag) {

  tmp <- extractAllDates(path_to_tar, tar_flag = tar_flag)

  # Select earliest date from the files.
  date_from <- min( tmp$allDates )
  date_to   <- max( tmp$allDates )

  return(list(date_from = date_from, date_to = date_to, timestep = tmp$timestep))
}

# Function for OS-Independently choosing a directory
choose_directory <- function(caption = 'Select data directory') {
  if (exists('utils::choose.dir') || exists('choose.dir')) {
    utils::choose.dir(caption = caption)
  } else {
    tcltk::tk_choose.dir(caption = caption)
  }
}

function(input, output, session) {
  #### Preparation and session set up ####
  # TODO: Setting the maximum request size. WARNING: NOT SURE WHAT'S A GOOD VALUE FOR THIS
  # FOR NOW SETTING TO 1.5 GB, as this exceeds the largest test data.
  options(shiny.maxRequestSize = 1500*1024^2)

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
    try( config_lines <- readLines(config_filepath) )
  } else {
    config_lines <- ""
  }

  # Save user dir as a global variable.
  userDir <<- ""
  outputDir <<- ""
  action_userDir_change <- reactiveVal(0)

  # 'Unique' session name
  sessionName <- paste0(session$user, format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"))

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

    # Set these values to one as soon as valid.
    userDirValid <- reactiveVal(0)
    resolutionValid <- reactiveVal(0)


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
          h4("Please select a directory where we can save your created output."),
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

      gridXsize <- 360/input$gridResolution + 1
      gridYsize <- 180/input$gridResolution + 1

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
    try( userDir <<- choose_directory() )
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

  # Reactive value for chosen operators + a simulated reactive value.
  operatorDataFrame <<- data.frame()
  operatorDataFrameAction <- reactiveVal(0)

  # Repeat message on file loss (remote case only)
  repeatWarning <- reactiveVal(TRUE)

  # Reactive value for the nc file to be used in analyze. (update action if file stays the same on new upload)
  nc_path_visualize <- reactiveVal()
  actionVisualize <- reactiveVal(0)

  # Shape file path
  shapeFile_path <- reactiveVal()
  shapeFile_path_action <- reactiveVal(0)

  # Storing the polygons.
  region_data <- reactiveVal()
  outfile_ctry <- reactiveVal()

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
  imageheight <- reactiveVal( round(image_def * (2 / 3)) )
  imagewidth  <- reactiveVal( image_def )
  lat_lon_trigger <- reactiveVal(0)

  # Reactive value to determine whether this was the first plot
  readyToPlot <- reactiveVal(FALSE)

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
    res <- try( tar_path(file.choose(new = TRUE)) )
    if (class(res) != "try-error") {
      #for (path in tar_path()){
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

  # Observing changes in selected tar file. (remote)
  observeEvent(input$tarFileRemote, {
    tar_path(input$tarFileRemote$name)
    if (!endsWith(tar_path(), ".tar")) {
      isolate(tar_path(""))
      wrong_file_modal(".tar")
    } else {
      tar_path_action(tar_path_action() + 1)
    }
  }, ignoreInit = TRUE)

  # Updating the path to the nc file analyze. (local)
  observeEvent(input$ncFileLocal_analyze, {
    shinyjs::disable("ncFileLocal_analyze")
    res <- try( nc_path_analyze(file.choose(new = TRUE)) )
    if (class(res) != 'try-error') {
      if (!endsWith(nc_path_analyze(), ".nc")) {
        isolate(nc_path_analyze(""))
        wrong_file_modal(".nc")
      } else {
        nc_path_analyze_action(nc_path_analyze_action() + 1)
      }
    }
    shinyjs::enable("ncFileLocal_analyze")
  }, ignoreInit = TRUE)

  # Observing changes in selected nc file analyze. (remote)
  observeEvent(input$ncFileRemote_analyze, {
    nc_path_analyze(input$ncFileRemote_analyze$name)
    if (!endsWith(nc_path_analyze(), ".nc")) {
      isolate(nc_path_analyze(""))
      wrong_file_modal(".nc")
    } else {
      nc_path_analyze_action(nc_path_analyze_action() + 1)
    }
  }, ignoreInit = TRUE)

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
    res <- try( nc_path_visualize(file.choose(new = TRUE)) )
    if (class(res) != 'try-error') {
      if (!endsWith(nc_path_visualize(), ".nc")) {
        isolate(nc_path_visualize(""))
        wrong_file_modal(".nc")
      } else {
        actionVisualize(actionVisualize() + 1)
      }
    }
    shinyjs::enable("ncFileLocal_visualize")
  }, ignoreInit = TRUE)

  # Observing changes in selected nc file visualize. (remote)
  observeEvent(input$ncFileRemote_visualize, {
    nc_path_visualize(input$ncFileRemote_visualize$name)
    if (!endsWith(nc_path_visualize(), ".nc")) {
      isolate(nc_path_visualize(""))
      wrong_file_modal(".nc")
    } else {
      actionVisualize(actionVisualize() + 1)
    }
  }, ignoreInit = TRUE)

  # If user chooses to take generated nc file update nc_path_visualize. (output file)
  observeEvent(input$useOutputFile_visualize, {
    nc_path_visualize(outputFilepath())
    if (!endsWith(nc_path_visualize(), ".nc")) {
      isolate(nc_path_visualize(""))
      wrong_file_modal(".nc")
    } else {
      actionVisualize( actionVisualize() + 1 )
    }
  }, ignoreInit = TRUE)

  # Updating path to shape file. (local)
  observeEvent(input$shapefileLocal, {
    shinyjs::disable("shapefileLocal")
    res <- try( shapeFile_path(file.choose(new = TRUE)) )
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
  observeEvent(input$shapefileRemote, {
    shapeFile_path(input$shapefileRemote$name)
    if (!endsWith(shapeFile_path(), ".shp")) {
      isolate(shapeFile_path(""))
      wrong_file_modal(".shp")
    } else {
      shapeFile_path_action(shapeFile_path_action() + 1)
    }
  }, ignoreInit = TRUE)

  # Deleting instat file path when removing plot r-instat check box selection
  observeEvent(input$plot_rinstat, {
    if (!input$plot_rinstat) {
      instat_path("")
    }
  })

  # Updating the path to the instat file. (local)
  observeEvent(input$instat_file_local, {
    shinyjs::disable("instat_file_local")
    res <- try( instat_path(file.choose(new = TRUE)) )
    if (class(res) != 'try-error') {
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
  observeEvent(input$instat_file_remote, {
    instat_path(input$instat_file_remote$name)
    if (!endsWith(instat_path(), ".RData")) {
      isolate(instat_path(""))
      wrong_file_modal(".RData")
    } else {
      instat_path_action(instat_path_action() + 1)
    }
  }, ignoreInit = TRUE)

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
    #for(path in tar_path()){
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
      extractedDates <- extractDateRange( tar_path() , tar_flag = 1)
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
    try( res <- globalAuxFilePath( file.choose(new = TRUE)) )
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
    try( res <- utils::download.file("https://public.cmsaf.dwd.de/data/perm/auxilliary_data/claas2_level2_aux_data.nc", auxfile, "auto", mode = "wb") )

    # Download file returns 0 on success.
    if (res == 0) {
      globalAuxFilePath( file.path( auxfile ) )
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
    # IN FACT: WE ARE OMITTING TIMESTEP VALUE CAUS WE DON'T APPEAR TO NEED IT.
    dates <- extractAllDates(path_to_tar, tar_flag = tar_flag)
    dates <- dates$allDates

    # Get claas_flag and strop from tarlist if needed.
    claas_flag <- getClaasAuxFlag(tarlist_all)
    if (claas_flag) {
      tarlist_all <- tarlist_all[tarlist_all != "CM_SAF_CLAAS2_L2_AUX.nc"]
    }

    if(tar_flag == 0){
      tar_chunk_size <- 100
    }else{
      tar_chunk_size <- 1000
    }

    # So filelist just equals tarlist here?
    filelist <- NULL

    ordname <- basename(path_to_tar)
    ordpath <- dirname(path_to_tar)

    flist <- list.files(ordpath,substr(ordname,1,8), full.names = TRUE)


    for (file in flist) {
      if (tar_flag == 1) {
        tarlist <- utils::untar(file,list = TRUE,tar = "internal")
      } else {
        tarlist <- utils::untar(file,list = TRUE)
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

        tarlist <- tarlist[start:end]

        filelist <- append(filelist,tarlist)

        if (length(tarlist) > tar_chunk_size) {
          dum <- seq(0, length(tarlist),tar_chunk_size)

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
      subset(vn,!(
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
        max_level <- length(ncdf4::ncvar_get(id,"lev"))
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

    untarVals( untarFiles(tar_path(), dateRange_prep()[1], dateRange_prep()[2], timestep(), tar_flag = 1) )
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
    file_info <- cmsaf:::check_dims(id)
    ncdf4::nc_close(id)

    if (!file_info$has_lon_lat) {
      if (claas_flag) {
        auxFilePath(file.path(ordPath, "CM_SAF_CLAAS2_L2_AUX.nc"))
        cmsaf::add_grid_info(infile, auxFilePath(), outfile = NULL, overwrite = TRUE)
      } else {
        grid_info <- cmsaf:::get_grid(infile)
        if (grid_info == 5 && (is.null(globalAuxFilePath()) || !file.exists(globalAuxFilePath())) ) {
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
        }
        cmsaf::add_grid_info(infile, globalAuxFilePath(), outfile = NULL, overwrite = TRUE)
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
      vars <- subset(userOptions$variables, !(userOptions$variables %in% c("lat","lon","time_bnds","nb2","time")))
      selectInput("variableInput",
                  "We found the following variables",
                  choices = vars)
    })

    # Rendering lat, lon range.
    output$lonRange_ui <- renderUI({
      sliderInput(inputId = "lonRange",
                  label = "Please select a longitude range.",
                  min = trunc(20*userOptions$lon_range[1])/20,
                  max = trunc(20*userOptions$lon_range[2])/20,
                  value = c(trunc(20*userOptions$lon_range[1])/20, trunc(20*userOptions$lon_range[2])/20),
                  step = 0.05
      )
    })

    # Rendering lat, lat range.
    output$latRange_ui <- renderUI({
      sliderInput(inputId = "latRange",
                  label = "Please select a latitude range.",
                  min = trunc(20*userOptions$lat_range[1])/20,
                  max = trunc(20*userOptions$lat_range[2])/20,
                  value = c(trunc(20*userOptions$lat_range[1])/20, trunc(20*userOptions$lat_range[2])/20),
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

    x_lims <- isolate(spatialRange$lon_range + c(-10, 10))
    y_lims <- isolate(spatialRange$lat_range + c(-10, 10))

    maps::map("world", fill = TRUE, col = "gray36", bg = "white", xlim = x_lims, ylim = y_lims)
    graphics::rect(input$lonRange[1], input$latRange[1], input$lonRange[2], input$latRange[2], lwd = 3, col = "brown4", density = 0)
    graphics::rect(input$lonRange[1], input$latRange[1], input$lonRange[2], input$latRange[2], lwd = 0.5, angle = 36, col = "brown4", density = 30)
    title(main = "Preview of available spatial coverage")
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
      pattern <- substr(filelist[1],1,5)

      outfile <- file.path(outputDir, paste0(var, "_", startDate, "-", endDate, ".nc"))

      cmsaf::levbox_mergetime(var,
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

      cmsaf::box_mergetime(var,
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

      file.remove(file.path(ordDir,removelist))
    }

    # Clean up variables
    var_list_default <- c("checkstring", "userDir", "date_from", "date_to", "dates",
                          "dates_all","delete", "dume", "dummy", "dums", "end", "endDate",
                          "filelist", "flist","gzcheck", "i", "id", "infile", "infile_name",
                          "n", "orddir", "ordname", "ordpath",
                          "outputDir", "pattern","slash", "split_path", "start",
                          "startDate", "tar_flag", "tarlist","tarlist_all", "timestep", "var",
                          "var_default", "vn","zipfile","t","claas_flag","sn","outputFormat",
                          "lon_var","lat_var","%ni%")
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
    repeatWarning( !input$noRepeat )
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
      ncdf4::nc_close(id)
      var_default <- subset(vn, !(vn %in% c("lat","lon","time_bnds","nb2","time")))

      # TODO: Change this as soon as sinusodial is done.
      # Stop if data are in sinusoidal projection
      if ("sinusoidal" %in% vn) {
        showModal(modalDialog(
          h4("Sorry, the CM SAF R Toolbox can not handle data in sinusoidal projection.
           Please use the 'change projection' option during the order process.
           Your NetCDF data have to be on a regular lon-lat grid."),
          title = "Error!",
          size = "l"))
      }

      output$usedVariable <- renderUI({
        selectInput("usedVariable",
                    label = "Please choose a variable",
                    choices = var_default,
                    width = "320px")
      })
      shinyjs::hide("panel_analyzeGo")
      shinyjs::show("panel_analyze")
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

    if(input$operatorGroup == "Selection"){

      if(input$operatorInput == "selyear"){
        nc <- ncdf4::nc_open(nc_path_analyze())
        time_info <- cmsaf:::get_date_time(ncdf4::ncvar_get(nc, "time"),ncdf4::ncatt_get(nc,"time","units")$value)
        years2 <- time_info$years
        ncdf4::nc_close(nc)

        output$years_to_select <- renderUI({
          selectInput("years",
                      label = "Select years",
                      choices = sort(unique(years2)),
                      width = "320px", multiple = TRUE)
        })
      }else if(input$operatorInput %in% c("selperiod","extract.period")){
        nc <- ncdf4::nc_open(nc_path_analyze())
        date_time <- as.Date(cmsaf::get_time(ncdf4::ncatt_get(nc,"time","units")$value,ncdf4::ncvar_get(nc, "time")))
        ncdf4::nc_close(nc)

        output$dateRange_to_select <- renderUI({
          dateRangeInput("dateRange_analyze",
                         label = "Select date range",
                         start = min(date_time),
                         end = max(date_time),
                         min = min(date_time),
                         max = max(date_time),
                         width = "320px")
        })
      }else if(input$operatorInput == "sellonlatbox"){
        nc <- ncdf4::nc_open(nc_path_analyze())
        lon <- ncdf4::ncvar_get(nc, "lon")
        lat <- ncdf4::ncvar_get(nc, "lat")
        ncdf4::nc_close(nc)

        output$region_to_select <- renderUI({
          tags$div(id = "region",
                   sliderInput("lonRegionSlider",
                               label = "Select longitude",
                               min = min(lon, na.rm = T),
                               max = max(lon, na.rm = T),
                               value = c(min(lon, na.rm = T), max(lon, na.rm = T)),
                               width = "320px"),
                   sliderInput("latRegionSlider",
                               label = "Select latitude",
                               min = min(lat, na.rm = T),
                               max = max(lat, na.rm = T),
                               value = c(min(lat, na.rm = T), max(lat, na.rm = T)),
                               width = "320px"))
        })
      }
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
        shinyjs::hide("years_to_select")
        shinyjs::hide("region_to_select")
      }
    }

    if (currentOperatorOption() != "None") {
      if(currentOperatorOption() == "dateRange"){
        shinyjs::show("dateRange_to_select")
      }else if(currentOperatorOption() == "years"){
        shinyjs::show("years_to_select")
      }else if(currentOperatorOption() == "region"){
        shinyjs::show("region_to_select")
      }else
      {
        shinyjs::show(currentOperatorOption())
      }
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
      cmsaf::ncinfo(nc_path_analyze())
    })

    if (nrow(operatorDataFrame) == 0) {
      shinyjs::hide("listOfOperators")
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

    if (newOutfile == nc_path_analyze() ) {
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
      argumentList <- list(var = input$usedVariable,
                           infile = nc_path_analyze(),
                           outfile = newOutfile,
                           lon1 = input$lonPoint,
                           lat1 = input$latPoint,
                           nc34 = input$format,
                           overwrite = TRUE)
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

      infile2 <- try( file.choose(new = TRUE) )

      if (class(infile2) == "try-error") {
        # Show modal and leave silently.
        showModal(modalDialog(
          h4("An additional file is required for this operator."),
          br(),
          h4("The grid information of infile2 are the target grid for the interpolation. This File may also be an ASCII-File containing the grid information."),
          br(),
          h4("For more information please type '?cmsaf::remap' in your R console."),
          title = "No remap file given.",
          size = "l"
        ))

        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")
        req( FALSE )
      } else if (!file.exists(infile2) || !(endsWith(infile2, ".nc") || endsWith(infile2, ".txt"))) {
        # Show modal and leave silently.
        showModal(modalDialog(
          h4("The additional file needs to be a", tags$strong(".nc"), " file (or", tags$strong(".txt"), " in case of ASCII-format)."),
          br(),
          h4("For more information please type '?cmsaf::remap' in your R console."),
          title = "No remap file given.",
          size = "l"
        ))

        # Hide spinner allow new operation and leave silently.
        shinyjs::hide("spinner_analyze")
        if (input$applyAnother) {
          shinyjs::show("listOfOperators", anim = TRUE, animType = "fade")
        }
        shinyjs::enable("applyOperator")

        req( FALSE )
      }

      argumentList <- list(var = input$usedVariable,
                           infile1 = nc_path_analyze(),
                           infile2 = infile2,
                           outfile = newOutfile,
                           method = input$method,
                           nc34 = input$format,
                           overwrite = TRUE)
    }

    res <- try(do.call(input$operatorInput, argumentList))
    # Error handling
    if (class(res) == "try-error") {
      showModal(modalDialog(
        h4("An error occured while applying the operator. Please use another input file or another operator."),
        tags$p(paste0("Message: ", res)),
        title = "Error!",
        size = "l"
      ))
    } else {
      # No error. Continue with rest.
      nc_path_analyze( newOutfile )
      nc_path_visualize( newOutfile )

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
                                                                     "lon: [", input$lonRegionSlider[1], " ", input$lonRegionSlider[2],"]"))
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

  #### VISUALIZE ####
  getVariableData <- function(timestep_index, id, var) {
    return(ncdf4::ncvar_get(id, var, start = c(1,1,timestep_index), count = c(-1,-1,1)))
  }

  # A function to read all required information from nc file
  # that's needed for the visualize options.
  # Also sets the correct image width and height.
  get_visualize_options <- function(infile, var) {
    # Open file and get data
    id <- ncdf4::nc_open(infile)
    # Remap to regGrid if necessary
    file_info <- cmsaf:::check_dims(id)
    ncdf4::nc_close(id)
    if (!file_info$isRegGrid) {
      remap_timestamp <- format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC")
      remap_name <- paste0("remap_", remap_timestamp, ".nc")
      outfile <- file.path(userDir, remap_name)
      # grid_filepath can be  found in global.R
      cmsaf::remap(var, infile, grid_filepath, outfile, overwrite = TRUE)
      infile <- outfile
      nc_path_visualize(infile)
    }

    # Open file and get data
    id <- ncdf4::nc_open(infile)

    lon <- ncdf4::ncvar_get(id, "lon")
    lat <- ncdf4::ncvar_get(id, "lat")

    data <- try( ncdf4::ncvar_get(id, var, collapse_degen = FALSE) )

    visualizeDataTimestep( getVariableData(1, id, var) )

    date <- ncdf4::ncvar_get(id, "time")
    t_unit <- ncdf4::ncatt_get(id, "time", "units")$value
    date.time <- as.character(cmsaf::get_time(t_unit, date))
    unit <- ncdf4::ncatt_get(id, var, "units")$value
    if (unit == 0)
      (unit <- "-")
    vn <- var
    varname <- ncdf4::ncatt_get(id, var, "long_name")$value
    if (varname == 0)
      (varname <- ncdf4::ncatt_get(id, var, "standard_name")$value)
    if (varname == 0)
      (varname <- var)

    creator_att <- ncdf4::ncatt_get(id,0,"creator_name")
    creator <- ifelse(creator_att$hasatt, creator_att$value, "DE/DWD")
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

      visualizeDataTimestep( visualizeDataTimestep()[rev(seq_len(length(lon))),] )
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
      visualizeDataTimestep( visualizeDataTimestep()[, rev(seq_len(length(lat)))] )

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

      visualizeDataMin( min(data, na.rm = TRUE) )
      visualizeDataMax( max(data, na.rm = TRUE) )
      x_range <- length(data)
      ltype <- c("l", "p", "o", "s", "h")
      date.time <- as.Date(date.time)
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

        visualizeDataMin( min_data )
        visualizeDataMax( max_data )
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

  # Function to update data min and max values

  # if (class(data) != "try-error") {
  #   min_data <- min(data, na.rm = TRUE)
  #   max_data <- max(data, na.rm = TRUE)
  # } else {
  #
  # }
  #
  # if (round(min_data, digits = 1) == round(max_data, digits = 1)) {
  #   min_data <- min_data - 0.05
  #   max_data <- max_data + 0.05
  # }
  #
  # visualizeDataMin( min_data )
  # visualizeDataMax( max_data )


  # Function for getting number of breaks.
  break_num <- function(ln, bn, minn, maxn, max_data) {
    dg <- 2
    if (abs(max_data) >= 10)
      (dg <- 1)
    if (abs(max_data) >= 100)
      (dg <- 0)
    a <- vector(mode = "character", length = bn)
    b <- round(seq(minn, maxn, length.out = bn), digits = dg)
    c <- round(seq(1, length(b), length.out = ln))
    a[c] <- b[c]
    labs <- a
    return(labs)
  }

  observeEvent(input$action_visualize_variable_modal, {
    # Update the variable
    variable_visualize_modal(input$variable_visualize_modal)

    # This will re-trigger the visualization process
    action_visualize_post_modal( action_visualize_post_modal() + 1)

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

    id <- ncdf4::nc_open(nc_path_visualize())
    vn <- names(id$var)
    ncdf4::nc_close(id)
    vn <- subset(vn, !(vn %in% c("lat","lon","time_bnds","nb2","time")))

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
      res <- try( visualizeVariables( get_visualize_options(nc_path_visualize(), vn) ) )

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
            tmp = c(max(round(visualizeVariables()$min_lon), -180), min(round(visualizeVariables()$max_lon), 180))
            lon_bounds( tmp )
            sliderInput("slider1",
                        label = "Longitude",
                        min = max(round(visualizeVariables()$min_lon) - 20, -180),
                        max = min(round(visualizeVariables()$max_lon) + 20, 180),
                        value = c(tmp[1], tmp[2]))
          })

          output$lat_visualize <- renderUI({
            tmp = c(max(round(visualizeVariables()$min_lat), -90), min(round(visualizeVariables()$max_lat), 90))
            lat_bounds( tmp )

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
                      value = paste0(visualizeVariables()$varname," [", visualizeVariables()$unit,"]"))
          })
        } else {
          # 1D-Plot

          # Hide Statistics panel
          shiny::hideTab(inputId = "mainVisualizeTabset", target = "Statistics")

          shinyjs::hide("sidebar_2d_plot")
          shinyjs::hide("myImage_2d")

          shinyjs::show("myImage_1d")
          shinyjs::show("sidebar_1d_plot")

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
    }

  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # Observe changes to lon, lat slider and update image plot width/height
  observeEvent({
    lon_bounds()
    lat_bounds()
  }, {

    req(visualizeVariables()$plot_dim == 2)

    lon <- visualizeVariables()$lon[visualizeVariables()$lon <= lon_bounds()[2]]
    lon <- lon[lon_bounds()[1] <= lon]

    lat <- visualizeVariables()$lat[visualizeVariables()$lat <= lat_bounds()[2]]
    lat <- lat[lat_bounds()[1] <= lat]

    # Update this value if you want to change min width/height of plot.
    minSize <- 200
    tmpWidth  <- max(minSize, image_def)
    tmpHeight <- max(minSize, image_def)

    # Update width and height according to visualizeVariables lat and lon vectors
    if (length(lon) >= length(lat)) {
      # Shrink height
      tmpHeight <- round( tmpWidth * length(lat) / length(lon))
      if (tmpHeight < minSize) {
        tmpWidth <- minSize/tmpHeight * tmpWidth
        tmpHeight <- minSize
      }

      # Why are we doing this? (And why not in the else block?)
      imageheight( tmpHeight + (round((ihsf * tmpHeight))) )
      imagewidth( tmpWidth )
    } else {
      # No need to check against minSize since we will multiply with a value > 1.
      tmpWidth <- round( tmpHeight * length(lat) / length(lon))

      imagewidth( tmpWidth )
      imageheight( tmpHeight )
    }

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
    req( visualizeDataMax() )
    req( visualizeDataMin() )

    sliderInput("slidery",
                label = "Y-Range",
                min = round(visualizeDataMin() - (0.25 * visualizeDataMax()), 1),
                max = round(visualizeDataMax() + (0.25 * visualizeDataMax()), 1),
                value = c(trunc(visualizeDataMin(), 1), ceiling(10*visualizeDataMax())/10))
  })

  # Observe changes to visualize data. If all data are available update min and max values globally.
  # Else we'll need to keep track of them
  observe({
    req( class(visualizeVariables()$data) == "try-error" )

    min_data <- min(visualizeDataTimestep(), na.rm = TRUE)
    max_data <- max(visualizeDataTimestep(), na.rm = TRUE)

    if (round(min_data, digits = 1) == round(max_data, digits = 1)) {
      min_data <- min_data - 0.05
      max_data <- max_data + 0.05
    }

    visualizeDataMin( min_data )
    visualizeDataMax( max_data )
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
        all_regions <- levels(countriesHigh[["ADMIN"]])

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
    lo_dummy <- c("lon","longitude","laenge","x")
    la_dummy <- c("lat","latitude","breite","y")
    ti_dummy <- c("time","date","zeit","t")
    da_dummy <- c("data","daten","z","element")

    dn <- attr(instat.data(),"element_name")
    if (!is.null(dn)) {
      da_dummy <- append(da_dummy,dn)
    } else {
      dn <- attr(instat.data(),"data_name")
      if (!is.null(dn)) {
        da_dummy <- append(da_dummy,dn)
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
      time_station <- instat.data()[,ti_n]
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
        match_time   <- which(format(as.Date(instat.data()[,ti_n]),"%Y-%m") == format(as.Date(input$timestep),"%Y-%m"),arr.ind = TRUE)
      } else {
        match_time   <- which(instat.data()[,ti_n] == input$timestep,arr.ind = TRUE)
      }

      lon_station  <- instat.data()[,lo_n][match_time]
      lat_station  <- instat.data()[,la_n][match_time]
      data_station <- instat.data()[,da_n][match_time]

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

          pos <- sp::SpatialPoints(cbind(lon_station[istation],lat_station[istation]), proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
          dum_dist <- 1000
          for (i in seq_along(lon2)) {
            for (j in seq_along(lat2)) {
              dist <- sp::spDistsN1(pos, c(lon2[i],lat2[j]), longlat = FALSE)
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
          data_sat[istation] <- visualizeDataTimestep()[lon_limit,lat_limit]
          # data_sat[istation] <- visualizeVariables()$data[lon_limit,lat_limit,which(visualizeVariables()$date.time == input$timestep,arr.ind = TRUE)]
        }
      }

      cd <- data.frame(data_station,data_sat,lon_station,lat_station)
      cd
    } # end if lo_n,la_n,ti_n,da_n
  })

   # Function for getting colors. Either the basic color schemes or from colorspace.
  getColors <- function(useColorspace = FALSE) {
      bpy <- eval(parse(text = "colorspace:::bpy"))
      idx <- which(rownames(palettes) == input$PAL)
      name   <- input$PAL
      curPAL <- as.list(palettes[idx,])
      if ( length(idx) == 0 ) {
        idx <- which(rownames(palettes) == "sunny")
        name   <- "sunny"
        curPAL <- as.list(palettes[idx,])
      }

      if ( curPAL$type == "base" ) {
        pal <- eval(parse(text = tolower(name)))
      } else if ( curPAL$type == "more" ) {
        sunny <- grDevices::colorRampPalette(c("black",
                                               "#3a0303",
                                               "#640000",
                                               "#981800",
                                               "#ca4b00",
                                               "#fc7f01",
                                               "#ffb234",
                                               "#ffe566",
                                               "#ffff98",
                                               "#ffffcb",
                                               "white"))
        tim.colors <- fields::tim.colors
        pal <- eval(parse(text = tolower(name)))
      } else {
        curPAL$reverse <- FALSE
        pal <- do.call(colorspace:::GetPalette, curPAL)
      }

      colorbar <- pal(input$num_brk)

      if ( input$reverse ) {
        colorbar <- rev(colorbar)
      }

      return(colorbar)    
  }
  # A function to create the plot in visualize.
  render_plot <- function(plot_rinstat) {
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = '.png')
    # Do not know where this is needed. Seems to me that it is not used.
    # breaks <- seq(input$num_rmin, input$num_rmax, length.out = (input$num_tick))
    tlab <- break_num(input$num_tick,
                      input$num_tick,
                      input$num_rmin,
                      input$num_rmax,
                      visualizeDataMax())
    xtick  <- grDevices::axisTicks(lon_bounds(), log = FALSE)
    ytick  <- grDevices::axisTicks(lat_bounds(), log = FALSE)
    xlab <-
      unlist(lapply(xtick, function(x)
        ifelse(x < 0,
               paste0(abs(x), " W"), ifelse(x > 0, paste0(abs(x), " E"), x))))
    ylab <-
      unlist(lapply(ytick, function(x)
        ifelse(
          x < 0, paste0(abs(x), " S"),
          ifelse(x > 0, paste0(abs(x), " N"), x)
        )))

    if (min(xtick) == round(lon_bounds()[1])) {
      xlab[1] <- " "
    }
    if (max(xtick) == round(lon_bounds()[2])) {
      xlab[length(xlab)] <- " "
    }
    if (min(ytick) == round(lat_bounds()[1])) {
      ylab[1] <- " "
    }
    if (max(ytick) == round(lat_bounds()[2])) {
      ylab[length(ylab)] <- " "
    }

    # If rectangular projection
    if (input$proj == "rect") {
      # Use colorspace pallete
      col <- getColors(useColorspace = input$useColorspace)

      iwidth  <- imagewidth()
      iheight <- imageheight()
      grDevices::png(outfile, width = iwidth, height = iheight)
      graphics::par(cex = textsize)

      # Only set if error in plot.new():Figure Margins too large
      par(mar = c(2,2,2.6,2))

      fields::image.plot(
        visualizeVariables()$lon,
        visualizeVariables()$lat,
        visualizeDataTimestep(),
        main = input$text1,
        xlab = " ",
        ylab = " ",
        xlim = lon_bounds(),
        ylim = lat_bounds(),
        zlim = c(input$num_rmin, input$num_rmax),
        col = col,
        axis.args = list(
          cex.axis = 1,
          at = as.numeric(tlab[tlab != ""]),
          labels = tlab[tlab != ""],
          mgp = c(1, 0.4, 0),
          tck = c(-0.3)
        ),
        legend.lab = input$text3,
        legend.line = -2,
        axes = FALSE
      )

      # na.color in global.R
      graphics::image(
        visualizeVariables()$lon,
        visualizeVariables()$lat,
        array(1:2, dim(visualizeDataTimestep())),
        xlab = " ",
        ylab = " ",
        col = na.color,
        axes = FALSE,
        xlim = lon_bounds(),
        ylim = lat_bounds(),
        add = TRUE
      )

      graphics::image(
        visualizeVariables()$lon,
        visualizeVariables()$lat,
        visualizeDataTimestep(),
        xlab = " ",
        ylab = " ",
        xlim = lon_bounds(),
        ylim = lat_bounds(),
        zlim = c(input$num_rmin, input$num_rmax),
        col = col,
        axes = FALSE,
        add = TRUE
      )

      if (!as.logical(input$int)) {
        maps::map(
          "world",
          add = TRUE,
          interior = FALSE,
          resolution = 0,
          col = bordercolor,
          lwd = linesize
        )
      }

      # linesize, bordercolor, plot_grid, and grid_col in global.R
      if (as.logical(input$int)) {
        raster::plot(world,
                     add = TRUE,
                     lwd = linesize,
                     col = bordercolor)
      }

      if (plot_grid) {
        graphics::grid(NULL, NULL, lty = 3, col = grid_col)
      }
      graphics::axis(
        1,
        mgp = c(0, -2.5, 0),
        tck = c(0.01),
        col.axis = bordercolor,
        cex.axis = 0.8 * textsize,
        at = xtick,
        labels = xlab
      )
      graphics::axis(
        2,
        mgp = c(0, -2.5, 0),
        tck = c(0.01),
        las = 1,
        col.axis = bordercolor,
        cex.axis = 0.8 * textsize,
        at = ytick,
        labels = ylab
      )
      graphics::box(col = bordercolor, lwd = linesize)

      if (input$location) {
        if (length(lon_loc_vec()) > 0 &&
            length(lon_loc_vec()) == length(lat_loc_vec()) &&
            length(lon_loc_vec()) == length(name_loc_vec())) {
          for (i in seq_along(lon_loc_vec())) {
            graphics::points(lon_loc_vec()[i],
                             lat_loc_vec()[i],
                             pch = 16,
                             col = bordercolor)
            graphics::text(
              lon_loc_vec()[i],
              lat_loc_vec()[i],
              name_loc_vec()[i],
              pos = 1,
              col = bordercolor,
              cex = textsize
            )
          }
        }
      }

      graphics::mtext(input$text2)
      graphics::mtext(visualizeVariables()$copyrightText, side = 1, adj = 1)

      # plot R-Instat
      if (plot_rinstat) {
        vec <- seq(input$num_rmin, input$num_rmax, length.out = input$num_brk + 1)
        data_station <- co.data()$data_station
        lon_station  <- co.data()$lon_station
        lat_station  <- co.data()$lat_station
        data_station[data_station >= input$num_rmax] <- input$num_rmax
        data_station[data_station <= input$num_rmin] <- input$num_rmin
        for (i in seq_along(data_station)) {
          point_col <- col[findInterval(data_station[i], vec, all.inside = TRUE)]
          graphics::points(
            lon_station[i],
            lat_station[i],
            pch = 21,
            bg = point_col,
            col = "gray30",
            cex = 3,
            lwd = 2
          )
        }
      }

      on.exit(grDevices::dev.off())
    }

    # If orthographic projection
    if (input$proj == "ortho") {
      # prepare plot
      ori  <- c(input$xort, input$yort, input$rort)             #orientation
      nx <- length(visualizeVariables()$lon)
      ny <- length(visualizeVariables()$lat)
      landcol  <- "navajowhite3"
      oceancol <- "cadetblue3"
      outcol   <- "cornsilk4"

      rep.row <- function(x, n) {
        matrix(rep(x, each = n), nrow = n)
      }

      lonv  <- replicate(length(visualizeVariables()$lat), visualizeVariables()$lon)
      latv  <- rep.row(visualizeVariables()$lat, length(visualizeVariables()$lon))
      datav <-
        as.vector(visualizeDataTimestep())

      a <-
        mapproj::mapproject(
          x = lonv,
          y = latv,
          projection = "orthographic",
          orientation = ori
        )
      m <- maps::map("world", plot = FALSE)

      # filter Nas
      if (sum(is.na(a$x)) > 0 | sum(is.na(a$y)) > 0) {
        dummy <- NULL
        dummy <- !is.na(a$x)
        a$x   <- a$x[dummy]
        a$y   <- a$y[dummy]
        datav <- datav[dummy]
        dummy <- NULL
        dummy <- !is.na(a$y)
        a$x   <- a$x[dummy]
        a$y   <- a$y[dummy]
        datav <- datav[dummy]
      }

      # define grid factors
      xr <- abs(range(visualizeVariables()$lon, na.rm = TRUE)[1]) + abs(range(visualizeVariables()$lon, na.rm = TRUE)[2])
      yr <- abs(range(visualizeVariables()$lat, na.rm = TRUE)[1]) + abs(range(visualizeVariables()$lat, na.rm = TRUE)[2])
      l1 <- 3.1  # max value for nx/xf
      l2 <- 2.0  # max value for ny/yf

      x1 <- c(40, 360)
      y1 <- c(1, l1)
      c1 <- stats::lm(y1 ~ x1)$coeff[[1]]
      c2 <- stats::lm(y1 ~ x1)$coeff[[2]]

      if (xr > 40 & xr <= 360) {
        xf <- c2 * xr + c1
        xf <- round(xf, digits = 1)
      } else {
        xf <- 1
      }

      x1 <- c(40, 180)
      y1 <- c(1, l2)
      c1 <- stats::lm(y1 ~ x1)$coeff[[1]]
      c2 <- stats::lm(y1 ~ x1)$coeff[[2]]

      if (yr > 40 & yr <= 180) {
        yf <- c2 * yr + c1
        yf <- round(yf, digits = 1)
      } else {
        yf <- 1
      }

      iwidth  <- 800
      iheight <- 800

      # Get colors
      pcol <- getColors(useColorspace = input$useColorspace)

      tlab <- break_num(input$num_tick,
                        input$num_tick,
                        input$num_rmin,
                        input$num_rmax,
                        visualizeDataMax())

      # Plot orthographic image
      grDevices::png(outfile, width = iwidth, height = iheight)

      fields::quilt.plot(
        a$x,
        a$y,
        datav,
        xlim = c(-1, 1),
        ylim = c(-1, 1),
        nx = nx / xf,
        ny = ny / yf,
        xlab = " ",
        ylab = " ",
        main = input$text1,
        col = pcol,
        axis.args = list(
          cex.axis = 1,
          at = as.numeric(tlab[tlab != ""]),
          labels = tlab[tlab != ""],
          mgp = c(1, 0.4, 0),
          tck = c(-0.3)
        ),
        legend.lab = input$text3,
        legend.line = -2,
        axes = FALSE
      )

      graphics::polygon(
        sin(seq(0, 2 * pi, length.out = 100)),
        cos(seq(0, 2 * pi, length.out = 100)),
        col = oceancol,
        border = grDevices::rgb(1, 1, 1, 0.5),
        lwd = 1
      )
      suppressWarnings(
        maps::map(
          "world",
          projection = "orthographic",
          orientation = ori,
          add = TRUE,
          interior = FALSE
          ,
          fill = TRUE,
          col = landcol,
          lwd = linesize,
          resolution = 0,
          border = NA
        )
      )
      fields::quilt.plot(
        a$x,
        a$y,
        datav,
        xlim = c(-1, 1),
        ylim = c(-1, 1),
        nx = nx / xf,
        ny = ny / yf,
        xlab = " ",
        ylab = " ",
        main = input$text1,
        col = pcol,
        axis.args = list(
          cex.axis = 1,
          at = as.numeric(tlab[tlab != ""]),
          labels = tlab[tlab != ""],
          mgp = c(1, 0.4, 0),
          tck = c(-0.3)
        ),
        legend.lab = input$text3,
        legend.line = -2,
        axes = FALSE,
        add = TRUE
      )
      # Plot borders
      if (!as.logical(input$int)) {
        suppressWarnings(
          maps::map(
            "world",
            projection = "orthographic",
            orientation = ori,
            add = TRUE,
            interior = FALSE,
            col = outcol,
            lwd = linesize,
            resolution = 0
          )
        )
      } else {
        suppressWarnings(
          maps::map(
            "world",
            projection = "orthographic",
            orientation = ori,
            add = TRUE,
            interior = TRUE,
            col = bordercolor,
            lwd = linesize,
            resolution = 0
          )
        )
      }
      if (plot_grid) {
        mapproj::map.grid(
          m,
          nx = 18,
          ny = 9,
          lty = 3,
          col = grid_col,
          cex = linesize
        )
      }
      graphics::mtext(input$text2)
      graphics::mtext(visualizeVariables()$copyrightText, side = 1, adj = 1)

      on.exit(grDevices::dev.off())
    }

    # Return a list containing the filename
    return(
      list(
        src = outfile,
        contentType = 'image/png',
        width = iwidth,
        height = iheight,
        alt = "This is alternate text"
      )
    )
  }

  # Function to create the country plot.
  getRegionPlot <- function(infile) {

    iwidth  <- imagewidth()
    iheight <- imageheight()
    outfile <- tempfile(fileext = '.png')

    if (input$division == "COUNTRY") {
      region <- countriesHigh[countriesHigh$ADMIN == input$region,]
    } else {
      region <- region_data()[region_data()[[input$division]] == input$region,]
    }

    grd <- sp::makegrid(region)
    grd_pts <- sp::SpatialPoints(coords = grd, proj4string = sp::CRS(sp::proj4string(region)))
    grd_pts_in <- grd_pts[region, ]

    lon1 <- min(grd[,1])
    lon2 <- max(grd[,1])
    lat1 <- min(grd[,2])
    lat2 <- max(grd[,2])

    # Is guaranteed to be of dimension 1 here.
    nc <- ncdf4::nc_open(infile)
    var <- names(nc$var)
    var <- subset(var, !(var %in% c("lat","lon","time_bnds","nb2","time","sig")))

    ncdf4::nc_close(nc)


    #nc_path_visualize_orig(nc_path_visualize)
    tmp_outfile_ctry <- file.path(userDir, paste(input$region, basename(nc_path_visualize()), sep = "_"))
    outfile_ctry(tmp_outfile_ctry)

    if(!file.exists(outfile_ctry())){
      cmsaf::sellonlatbox(var,infile,tmp_outfile_ctry,lon1,lon2,lat1,lat2,overwrite = TRUE)
    }

    nc <- ncdf4::nc_open(tmp_outfile_ctry)

    lon1 <- sp::coordinates(grd_pts_in)[,1]
    lat1 <- sp::coordinates(grd_pts_in)[,2]

    lon <- ncdf4::ncvar_get(nc,"lon")
    lat <- ncdf4::ncvar_get(nc,"lat")

    #target <- ncdf4::ncvar_get(nc, var)

    dlon <- 0.05
    dlat <- 0.05

    out <- NULL

    target_lon  <- NULL
    target_lat  <- NULL
    target_x <- NULL
    target_y <- NULL
    target_data <- NULL

    target2 <- array(NA, dim = c(length(lon),length(lat)))

    dlon <- abs(lon[1] - lon[2])
    dlat <- abs(lat[1] - lat[2])

    for (n in seq_along(lon1)) {
      lon_limit <- which(lon >= (lon1[n] - dlon) & lon <= (lon1[n] + dlon))
      lat_limit <- which(lat >= (lat1[n] - dlat) & lat <= (lat1[n] + dlat))

      if (!(any(lon_limit) & any(lat_limit))) {
        out <- append(out,n)
        next()
      }

      lon2 <- lon[lon_limit]
      lat2 <- lat[lat_limit]

      pos <- sp::SpatialPoints(cbind(lon1[n],lat1[n]), proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
      dum_dist <- 1000
      for (i in seq_along(lon2)) {
        for (j in seq_along(lat2)) {
          dist <- sp::spDistsN1(pos, c(lon2[i],lat2[j]), longlat = FALSE)
          if (dist <= dum_dist) {
            dum_dist <- dist
            dumi <- i
            dumj <- j
          }
        }
      }

      lon_limit <- which(lon == lon2[dumi])
      lat_limit <- which(lat == lat2[dumj])

      if (!(any(lon_limit) & any(lat_limit))){
        stop("Coordinates outside of the domain.")
      }

      target_lon <- append(target_lon,lon[lon_limit])
      target_lat <- append(target_lat,lat[lat_limit])
      target_x <- append(target_x,lon_limit)
      target_y <- append(target_y,lat_limit)

      time_index <- match(input$timestep, visualizeVariables()$date.time)

      target <- ncdf4::ncvar_get(nc,var,start = c(lon_limit,lat_limit,time_index),count = c(1,1,1))
      target_data <- append(target_data,target)
    } # end for

    ncdf4::nc_close(nc)
    # file.remove(tmp_outfile_ctry)

    Longitude <- sort(unique(target_lon))
    Latitude <- sort(unique(target_lat))

    dm <- cbind(target_x,sort(target_y))
    target2[dm] <- target_data
    target3 <- target2[which(lon %in% target_lon),which(lat %in% target_lat)]

    col <- getColors(useColorspace = input$useColorspace)
    
    grDevices::png(outfile, width = iwidth, height = iheight)
    # Only set if error in plot.new():Figure Margins too large
    graphics::par(mar = c(2.5,2,2.6,1))

    fields::image.plot(Longitude,
                       Latitude,
                       target3,
                       main = input$text1,
                       zlim = c(input$num_rmin, input$num_rmax),
                       col = col,
                       legend.lab = input$text3,
                       legend.line = -2,
                       axes = FALSE)
    if (plot_grid) {
      graphics::grid(NULL, NULL, lty = 3, col = grid_col)
    }

    graphics::mtext(input$text2)
    graphics::mtext(visualizeVariables()$copyrightText, side = 1, adj = 1)

    raster::plot(region, add = TRUE)

    if (input$location) {
      if (length(lon_loc_vec()) > 0 &&
          length(lon_loc_vec()) == length(lat_loc_vec()) &&
          length(lon_loc_vec()) == length(name_loc_vec())) {
        for (i in seq_along(lon_loc_vec())) {
          graphics::points(lon_loc_vec()[i],
                           lat_loc_vec()[i],
                           pch = 16,
                           col = bordercolor)

          graphics::text(
            lon_loc_vec()[i],
            lat_loc_vec()[i],
            name_loc_vec()[i],
            pos = 1,
            col = bordercolor,
            cex = textsize
          )
        }
      }
    }
    on.exit(grDevices::dev.off())

    # Return a list containing the filename
    return(
      list(
        src = outfile,
        contentType = 'image/png',
        width = iwidth,
        height = iheight,
        alt = "This is alternate text"
      )
    )
  }

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
      lon_loc_vec( append(lon_loc_vec(),input$lon_loc) )
      lat_loc_vec( append(lat_loc_vec(),input$lat_loc) )
      name_loc_vec( append(name_loc_vec(),input$name_loc) )
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
    visualizeDataTimestep( tmp )
  })

  ## Colorpalette Stuff ##
  palettes <- colorspace:::GetPaletteConfig(gui = TRUE)

  observeEvent(input$typ, {
    x <- list()
    if ( grepl("^base$", input$typ) ) {
      shinyjs::disable("registerpalettebutton")
      shinyjs::disable("registerpalettename")
    } else {
      shinyjs::enable("registerpalettebutton")
      shinyjs::enable("registerpalettename")
    }
    for ( i in which(palettes$typ == input$typ) )
      x[[sprintf("%s",rownames(palettes)[i])]] <- rownames(palettes)[i]
    updateSelectInput(session, "PAL", choices = x)
  })

  # ----------------------------------------------------------------
  # Getting currently selected color scheme
  # ----------------------------------------------------------------
  palettes <- colorspace:::GetPaletteConfig(gui = TRUE)
  names(palettes) <- tolower(names(palettes))
  names(palettes)[names(palettes) == 'typ'] <- "type"

  # add more color schemes
  new_row <- data.frame("more",NA,NA,NA,NA,NA,NA,NA,NA,NA,1)
  names(new_row) <- names(palettes)
  palettes <- rbind(palettes,new_row)
  rownames(palettes)[75] <- "tim.colors"

  palettes <- rbind(palettes,new_row)
  rownames(palettes)[76] <- "sunny"

  x <- list()
  for (i in 1:nrow(palettes)) {
    x[[sprintf("%s",rownames(palettes)[i])]] <- rownames(palettes)[i]
    updateSelectInput(session, "PAL", choices = x,selected = "sunny")
  }

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

  # A reactive, throttled function for generating the plot.
  getPlot_2d <- reactive({
    req(readyToPlot())

    # Required triggers
    req(nrow(db_visualizeDataTimestep()) > 0)  # when data at timestep changes
    req(nchar(db_text1()) > 0)                        # Need a title
    req(db_proj())                         # projection
    req(db_visualizeDataMax())             # max data (not sure why want to trigger this?)

    # Isolated requirements
    isolate( req(lon_bounds()) )    # Require this to prevent error message
    isolate( req(lat_bounds()) )    # However, do not trigger on change
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
      if ( validity$valid != TRUE ) {
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
        res <- try( ls <- getRegionPlot(infile = isolate(nc_path_visualize())) )
      } else {
        res <- try( ls <- render_plot(plot_rinstat = plot_rinstat) )
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
        size = "l",
      ))

      # Silently leave.
      req(FALSE)
    }

    # A temp file to save the output.
    outfile <- tempfile(fileext = '.png')

    isolate({
      # prepare ticks and date formats
      dum_tick <- seq(1,length(visualizeVariables()$date.time), length.out = input$ticknumber)
      dum_tick2 <- NULL
      for (j in 2:length(dum_tick)) {
        dummy <- seq(dum_tick[j - 1], dum_tick[j], length.out = 4)
        if (j > 2 & j != length(dum_tick)) (dummy <- dummy[2:4])
        dum_tick2 <- append(dum_tick2,dummy)
      }
      if (input$dateformat == 1) (date.lab <- format(visualizeVariables()$date.time[dum_tick], "%Y"))
      if (input$dateformat == 2) (date.lab <- format(visualizeVariables()$date.time[dum_tick], "%Y-%m"))
      if (input$dateformat == 3) (date.lab <- format(visualizeVariables()$date.time[dum_tick], "%Y-%m-%d"))

      if (as.logical(input$analyze_timeseries)) {
        # Set the size of the output window
        iwidth  <- 650
        iheight <- 800

        grDevices::png(outfile, width = iwidth, height = iheight)
        # Analyze Timeseries

        # Set the number of rows and columns
        nrow <- 3
        ncol <- 2

        field <- visualizeVariables()$data[,,input$sliderx[1]:input$sliderx[2]]
        date.time2 <- visualizeVariables()$date.time[input$sliderx[1]:input$sliderx[2]]

        # Create vectors of the months and years, respectively
        timemonth <- format(date.time2, "%m")
        timeyear <- format(date.time2, "%Y")
        nt <- length(date.time2)

        # Create vectors of the months and years, respectively
        timemonth.in <- format(date.time2,"%m")
        timeyear.in <- format(date.time2,"%Y")
        startyear <- timeyear.in[1]
        startmonth <- timemonth.in[1]

        title <- visualizeVariables()$varname
        varlabel <- paste0(title," [", visualizeVariables()$unit,"]")

        # The function tapply is very useful for operations that
        # operate along the time axis, e.g., calculating mean monthly values
        field.monmean <- tapply(field,timemonth,mean,na.rm = TRUE)
        field.monmax <- tapply(field,timemonth,max,na.rm = TRUE)
        field.monmin <- tapply(field,timemonth,min,na.rm = TRUE)
        field.monsd <- tapply(field,timemonth,stats::sd,na.rm = TRUE)

        # Annual mean is only calculated if data for all 12 month are available
        field.annmean <- tapply(field,timeyear,mean,na.rm = TRUE)
        # Include only those years with 12 months on data
        years <- dimnames(field.annmean)[[1]]
        nyears <- length(years)
        nmonth <- vector(mode = "numeric",length = nyears)
        for (i in 1:nyears) {
          nmonth[i] <- length(which(timeyear == years[i]))
        }
        ind <- which(nmonth < 12)
        field.annmean[ind] <- NA

        # Calculate anomalies
        field.ano <- vector(mode = "numeric",length = nt)
        field.relano <- vector(mode = "numeric",length = nt)
        for (j in 1:nt) {
          field.ano[j] <- field[j] - field.monmean[timemonth[j]]
          field.relano[j] <- field.ano[j]/field.monsd[timemonth[j]]
        }

        # set the number of rows and columns of the plot
        graphics::par(mfrow = c(nrow,ncol))

        # Determine the min and max of the plotrange
        pmin <- min(field,na.rm = TRUE)
        pmax <- max(field,na.rm = TRUE)
        drange <- pmax - pmin

        #plot the data
        graphics::plot(date.time2,field, ylab = varlabel, xlab = "",
                       main = paste0(title,", ",format(visualizeVariables()$lat,digits = 4,nsmall = 2)," N, ",
                                     format(visualizeVariables()$lon,digits = 4,nsmall = 2)," E"),
                       ylim = c(pmin,pmax),type = "l")

        # calculate the linear trend of the original data
        x <- c(1:nt)
        model <- stats::lm(field~x,na.action = stats::na.exclude)
        graphics::lines(date.time2,stats::predict(model),col = input$integer, lwd = 2.0)
        conf <- stats::confint(model, "x", level = 0.95)
        trend <- model$coeff["x"] * 12.
        lconf <- conf[1] * 12.
        uconf <- conf[2] * 12

        mean.out <- format(mean(field, na.rm = TRUE), digits = 3, nsmall =
                             1)
        trend.out <-
          paste0(
            "[",
            format(lconf, digits = 2, nsmall = 2),
            ",",
            format(trend, digits = 2, nsmall = 2),
            ",",
            format(uconf, digits = 2, nsmall = 2),
            "] "
          )

        # Determine the x-location of the text in date format
        xtext <- as.Date(paste(startyear, startmonth, 01, sep = "-"))

        # textsize can be found in global.R
        graphics::text(
          xtext,
          pmax - 0.01 * drange,
          paste("mean:", mean.out, visualizeVariables()$unit),
          pos = 4,
          cex = textsize
        )
        graphics::text(
          xtext,
          pmax - 0.08 * drange,
          paste0("linear trend:", trend.out, visualizeVariables()$unit, "/yr"),
          pos = 4,
          cex = textsize
        )

        #--------------------------------------------------
        #Plot the monthly mean seasonal cycle
        # Changed this line (used to be months <- 1:12 but they don't always all exist.)
        months <- unique(names(field.monmax))
        # Determine the min and max of the plotrange
        pmin <- min(field.monmin, na.rm = TRUE)
        pmax <- max(field.monmax, na.rm = TRUE)

        graphics::plot(
          months,
          field.monmax,
          type = "n",
          main = "Average Seasonal Cycle",
          ylab = varlabel,
          xlab = "Months",
          ylim = c(pmin, pmax)
        )
        graphics::lines(months, field.monmax)
        graphics::lines(months, field.monmin)
        graphics::polygon(c(months, rev(months)), c(field.monmin, rev(field.monmax)), col =
                            input$integer)
        graphics::lines(months, field.monmean, lwd = 2)

        #--------------------------------------------------
        # Plot the monthly anomalies
        pmin <- min(field.ano, na.rm = TRUE)
        pmax <-
          max(field.ano, na.rm = TRUE) + 0.15 * (max(field.ano, na.rm = TRUE) - min(field.ano, na.rm =
                                                                                      TRUE))
        drange <- pmax - pmin
        graphics::plot(
          date.time2,
          field.ano,
          ylab = varlabel,
          xlab = "",
          main = "Monthly anomalies",
          ylim = c(pmin, pmax),
          type = "l"
        )
        graphics::abline(h = 0, lwd = 1.0, col = "gray40")

        # calculate the linear trend of the anomaly data
        x <- c(1:nt)
        model <- stats::lm(field.ano ~ x, na.action = stats::na.exclude)
        graphics::lines(date.time2,
                        stats::predict(model),
                        col = input$integer,
                        lwd = 2.0)
        conf <- stats::confint(model, "x", level = 0.95)
        trend <- model$coeff["x"] * 12.
        lconf <- conf[1] * 12.
        uconf <- conf[2] * 12

        trend.out <-
          paste0(
            "[",
            format(lconf, digits = 2, nsmall = 2),
            ",",
            format(trend, digits = 2, nsmall = 2),
            ",",
            format(uconf, digits = 2, nsmall = 2),
            "] "
          )

        # Determine the x-location of the text in date format
        xtext <- as.Date(paste(startyear, startmonth, 01, sep = "-"))

        graphics::text(
          xtext,
          pmax - 0.01 * drange,
          paste0("linear trend:", trend.out, visualizeVariables()$unit, "/yr"),
          pos = 4,
          cex = textsize
        )


        #--------------------------------------------------
        # Boxplot of the time series
        # Define the months as a categorical variable
        month_cat <- factor(timemonth)
        graphics::plot(month_cat,
                       field,
                       main = "Box Plot",
                       ylab = varlabel,
                       xlab = "Months")

        #--------------------------------------------------
        # Plot the annual means
        graphics::plot(
          as.integer(years) + 0.5,
          field.annmean,
          type = "p",
          main = "Annual Means",
          ylab = varlabel,
          xlab = "",
          pch = 19
        )
        graphics::abline(h = mean(field.annmean, na.rm = TRUE),
                         lwd = 1.0,
                         col = "gray40")

        #--------------------------------------------------
        # Plot a histogram of the data
        hist_field <- graphics::hist(
          field,
          breaks = 20,
          xlab = varlabel,
          main = paste0("Histogram of ", title),
          col = input$integer
        )
        graphics::box(col = "gray20", lwd = 1)

        on.exit(grDevices::dev.off())
      } else {
        # Generate the PNG with different line types

        # In the following textsize, and linesize can be found in global.R
        if (input$checkGroup_type == 1) {
          iwidth  <- imagewidth()
          iheight <- imageheight()
          grDevices::png(outfile, width = iwidth, height = iheight)
          graphics::par(cex = textsize)
          graphics::plot(
            visualizeVariables()$date.time,
            visualizeVariables()$data,
            type = "l",
            xlim = visualizeVariables()$date.time[input$sliderx],
            ylim = input$slidery,
            col = "white",
            main = input$text1_1d,
            xlab = "time",
            ylab = visualizeVariables()$ylabel,
            axes = FALSE
          )
          graphics::abline(h = 0, lwd = 1, col = "gray")
          graphics::grid(NA, NULL, lwd = 0.8)
          graphics::abline(
            v = visualizeVariables()$date.time,
            col = "lightgray",
            lty = 3,
            lwd = 0.8
          )
          graphics::points(
            visualizeVariables()$date.time,
            visualizeVariables()$data,
            type = "l",
            xlim = visualizeVariables()$date.time[input$sliderx],
            ylim = input$slidery,
            col = input$integer,
            lwd = linesize
          )
          graphics::axis(
            side = 1,
            at = visualizeVariables()$date.time[dum_tick],
            labels = date.lab,
            tck = -0.025,
            col.ticks = "gray20",
            col.axis = "gray20"
          )
          graphics::axis(
            side = 1,
            at = visualizeVariables()$date.time[dum_tick],
            labels = FALSE,
            tck = 0.015,
            col.ticks = "gray20",
            col.axis = "gray20"
          )
          graphics::rug(
            x = visualizeVariables()$date.time[dum_tick2],
            ticksize = 0.015,
            side = 1,
            quiet = TRUE
          )
          graphics::axis(
            side = 2,
            tck = -0.025,
            col.ticks = "gray20",
            col.axis = "gray20"
          )
          graphics::axis(
            side = 2,
            tck = 0.015,
            col.ticks = "gray20",
            col.axis = "gray20",
            labels = FALSE
          )
          graphics::box(col = "gray20", lwd = 1)
          if (as.logical(input$trend)) {
            # calculate the linear trend
            x <-
              c(seq_along(visualizeVariables()$date.time[input$sliderx[1]:input$sliderx[2]]))
            model <-
              stats::lm(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]] ~ x, na.action = stats::na.exclude)
            graphics::lines(visualizeVariables()$date.time[input$sliderx[1]:input$sliderx[2]],
                            stats::predict(model),
                            col = "gray20",
                            lwd = linesize)
            conf <- stats::confint(model, "x", level = 0.95)
            trend <- model$coeff["x"] * 12.
            lconf <- conf[1] * 12.
            uconf <- conf[2] * 12

            mean.out <-
              format(mean(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]], na.rm = TRUE),
                     digits = 3,
                     nsmall = 1)
            trend.out <-
              paste0(
                "[",
                format(lconf, digits = 2, nsmall = 2),
                ",",
                format(trend, digits = 2, nsmall = 2),
                ",",
                format(uconf, digits = 2, nsmall = 2),
                "] "
              )
            xtext <- as.Date(visualizeVariables()$date.time[input$sliderx[1]])

            pmin <-
              min(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]], na.rm = TRUE)
            pmax <-
              max(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]], na.rm = TRUE)
            drange <- pmax - pmin
            graphics::text(
              xtext,
              pmax - 0.01 * drange,
              paste("mean:", mean.out, visualizeVariables()$unit),
              pos = 4,
              cex = textsize
            )
            graphics::text(
              xtext,
              pmax - 0.08 * drange,
              paste0("linear trend:", trend.out, visualizeVariables()$unit, "/yr"),
              pos = 4,
              cex = textsize
            )
          }
          graphics::mtext(input$text2_1d)
          on.exit(grDevices::dev.off())
        }

        if (input$checkGroup_type == 2) {
          iwidth  <- imagewidth()
          iheight <- imageheight()
          grDevices::png(outfile, width = iwidth, height = iheight)
          graphics::par(cex = textsize)
          graphics::plot(
            visualizeVariables()$date.time,
            visualizeVariables()$data,
            type = "p",
            xlim = visualizeVariables()$date.time[input$sliderx],
            ylim = input$slidery,
            col = "white",
            main = input$text1_1d,
            xlab = "time",
            ylab = visualizeVariables()$ylabel,
            axes = FALSE
          )
          graphics::abline(h = 0, lwd = 1, col = "gray")
          graphics::grid(NA, NULL, lwd = 0.8)
          graphics::abline(
            v = visualizeVariables()$date.time,
            col = "lightgray",
            lty = 3,
            lwd = 0.8
          )
          graphics::points(
            visualizeVariables()$date.time,
            visualizeVariables()$data,
            type = "p",
            xlim = visualizeVariables()$date.time[input$sliderx],
            ylim = input$slidery,
            col = input$integer,
            lwd = linesize
          )
          graphics::axis(
            side = 1,
            at = visualizeVariables()$date.time[dum_tick],
            labels = date.lab,
            tck = -0.025,
            col.ticks = "gray20",
            col.axis = "gray20"
          )
          graphics::axis(
            side = 1,
            at = visualizeVariables()$date.time[dum_tick],
            labels = FALSE,
            tck = 0.015,
            col.ticks = "gray20",
            col.axis = "gray20"
          )
          graphics::rug(
            x = visualizeVariables()$date.time[dum_tick2],
            ticksize = 0.015,
            side = 1,
            quiet = TRUE
          )
          graphics::axis(
            side = 2,
            tck = -0.025,
            col.ticks = "gray20",
            col.axis = "gray20"
          )
          graphics::axis(
            side = 2,
            tck = 0.015,
            col.ticks = "gray20",
            col.axis = "gray20",
            labels = FALSE
          )
          graphics::box(col = "gray20", lwd = 1)
          if (as.logical(input$trend)) {
            # calculate the linear trend
            x <-
              c(seq_along(visualizeVariables()$date.time[input$sliderx[1]:input$sliderx[2]]))
            model <-
              stats::lm(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]] ~ x, na.action = stats::na.exclude)
            graphics::lines(visualizeVariables()$date.time[input$sliderx[1]:input$sliderx[2]],
                            stats::predict(model),
                            col = "gray20",
                            lwd = linesize)
            conf <- stats::confint(model, "x", level = 0.95)
            trend <- model$coeff["x"] * 12.
            lconf <- conf[1] * 12.
            uconf <- conf[2] * 12

            mean.out <-
              format(mean(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]], na.rm = TRUE),
                     digits = 3,
                     nsmall = 1)
            trend.out <-
              paste0(
                "[",
                format(lconf, digits = 2, nsmall = 2),
                ",",
                format(trend, digits = 2, nsmall = 2),
                ",",
                format(uconf, digits = 2, nsmall = 2),
                "] "
              )
            xtext <- as.Date(visualizeVariables()$date.time[input$sliderx[1]])

            pmin <-
              min(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]], na.rm = TRUE)
            pmax <-
              max(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]], na.rm = TRUE)
            drange <- pmax - pmin
            graphics::text(
              xtext,
              pmax - 0.01 * drange,
              paste("mean:", mean.out, visualizeVariables()$unit, sep = " "),
              pos = 4,
              cex = textsize
            )
            graphics::text(
              xtext,
              pmax - 0.08 * drange,
              paste0("linear trend:", trend.out, visualizeVariables()$unit, "/yr"),
              pos = 4,
              cex = textsize
            )
          }
          graphics::mtext(input$text2_1d)
          on.exit(grDevices::dev.off())
        }

        if (input$checkGroup_type == 3) {
          iwidth  <- imagewidth()
          iheight <- imageheight()
          grDevices::png(outfile, width = iwidth, height = iheight)
          graphics::par(cex = textsize)
          graphics::plot(
            visualizeVariables()$date.time,
            visualizeVariables()$data,
            type = "o",
            xlim = visualizeVariables()$date.time[input$sliderx],
            ylim = input$slidery,
            col = "white",
            main = input$text1_1d,
            xlab = "time",
            ylab = visualizeVariables()$ylabel,
            axes = FALSE
          )
          graphics::abline(h = 0, lwd = 1, col = "gray")
          graphics::grid(NA, NULL, lwd = 0.8)
          graphics::abline(
            v = visualizeVariables()$date.time,
            col = "lightgray",
            lty = 3,
            lwd = 0.8
          )
          graphics::points(
            visualizeVariables()$date.time,
            visualizeVariables()$data,
            type = "o",
            xlim = visualizeVariables()$date.time[input$sliderx],
            ylim = input$slidery,
            col = input$integer,
            lwd = linesize
          )
          graphics::axis(
            side = 1,
            at = visualizeVariables()$date.time[dum_tick],
            labels = date.lab,
            tck = -0.025,
            col.ticks = "gray20",
            col.axis = "gray20"
          )
          graphics::axis(
            side = 1,
            at = visualizeVariables()$date.time[dum_tick],
            labels = FALSE,
            tck = 0.015,
            col.ticks = "gray20",
            col.axis = "gray20"
          )
          graphics::rug(
            x = visualizeVariables()$date.time[dum_tick2],
            ticksize = 0.015,
            side = 1,
            quiet = TRUE
          )
          graphics::axis(
            side = 2,
            tck = -0.025,
            col.ticks = "gray20",
            col.axis = "gray20"
          )
          graphics::axis(
            side = 2,
            tck = 0.015,
            col.ticks = "gray20",
            col.axis = "gray20",
            labels = FALSE
          )
          graphics::box(col = "gray20", lwd = 1)
          if (as.logical(input$trend)) {
            # calculate the linear trend
            x <-
              c(seq_along(visualizeVariables()$date.time[input$sliderx[1]:input$sliderx[2]]))
            model <-
              stats::lm(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]] ~ x, na.action = stats::na.exclude)
            graphics::lines(visualizeVariables()$date.time[input$sliderx[1]:input$sliderx[2]],
                            stats::predict(model),
                            col = "gray20",
                            lwd = linesize)
            conf <- stats::confint(model, "x", level = 0.95)
            trend <- model$coeff["x"] * 12.
            lconf <- conf[1] * 12.
            uconf <- conf[2] * 12

            mean.out <-
              format(mean(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]], na.rm = TRUE),
                     digits = 3,
                     nsmall = 1)
            trend.out <-
              paste0(
                "[",
                format(lconf, digits = 2, nsmall = 2),
                ",",
                format(trend, digits = 2, nsmall = 2),
                ",",
                format(uconf, digits = 2, nsmall = 2),
                "] "
              )
            xtext <- as.Date(visualizeVariables()$date.time[input$sliderx[1]])

            pmin <-
              min(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]], na.rm = TRUE)
            pmax <-
              max(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]], na.rm = TRUE)
            drange <- pmax - pmin
            graphics::text(
              xtext,
              pmax - 0.01 * drange,
              paste("mean:", mean.out, visualizeVariables()$unit, sep = " "),
              pos = 4,
              cex = textsize
            )
            graphics::text(
              xtext,
              pmax - 0.08 * drange,
              paste0("linear trend:", trend.out, visualizeVariables()$unit, "/yr"),
              pos = 4,
              cex = textsize
            )
          }
          graphics::mtext(input$text2_1d)
          on.exit(grDevices::dev.off())
        }

        if (input$checkGroup_type == 4) {
          iwidth  <- imagewidth()
          iheight <- imageheight()
          grDevices::png(outfile, width = iwidth, height = iheight)
          graphics::par(cex = textsize)
          graphics::plot(
            visualizeVariables()$date.time,
            visualizeVariables()$data,
            type = "s",
            xlim = visualizeVariables()$date.time[input$sliderx],
            ylim = input$slidery,
            col = "white",
            main = input$text1_1d,
            xlab = "time",
            ylab = visualizeVariables()$ylabel,
            axes = FALSE
          )
          graphics::abline(h = 0, lwd = 1, col = "gray")
          graphics::grid(NA, NULL, lwd = 0.8)
          graphics::abline(
            v = visualizeVariables()$date.time,
            col = "lightgray",
            lty = 3,
            lwd = 0.8
          )
          graphics::points(
            visualizeVariables()$date.time,
            visualizeVariables()$data,
            type = "s",
            xlim = visualizeVariables()$date.time[input$sliderx],
            ylim = input$slidery,
            col = input$integer,
            lwd = linesize
          )
          graphics::axis(
            side = 1,
            at = visualizeVariables()$date.time[dum_tick],
            labels = date.lab,
            tck = -0.025,
            col.ticks = "gray20",
            col.axis = "gray20"
          )
          graphics::axis(
            side = 1,
            at = visualizeVariables()$date.time[dum_tick],
            labels = FALSE,
            tck = 0.015,
            col.ticks = "gray20",
            col.axis = "gray20"
          )
          graphics::rug(
            x = visualizeVariables()$date.time[dum_tick2],
            ticksize = 0.015,
            side = 1,
            quiet = TRUE
          )
          graphics::axis(
            side = 2,
            tck = -0.025,
            col.ticks = "gray20",
            col.axis = "gray20"
          )
          graphics::axis(
            side = 2,
            tck = 0.015,
            col.ticks = "gray20",
            col.axis = "gray20",
            labels = FALSE
          )
          graphics::box(col = "gray20", lwd = 1)
          if (as.logical(input$trend))
            (graphics::points(visualizeVariables()$date.time,
                              fitted,
                              type = "l",
                              col = "gray20",
                              lwd = linesize))
          graphics::mtext(input$text2_1d)
          on.exit(grDevices::dev.off())
        }

        if (input$checkGroup_type == 5) {
          iwidth  <- imagewidth()
          iheight <- imageheight()
          grDevices::png(outfile, width = iwidth, height = iheight)
          graphics::par(cex = textsize)
          graphics::plot(
            visualizeVariables()$date.time,
            visualizeVariables()$data,
            type = "h",
            xlim = visualizeVariables()$date.time[input$sliderx],
            ylim = input$slidery,
            col = "white",
            main = input$text1_1d,
            xlab = "time",
            ylab = visualizeVariables()$ylabel,
            axes = FALSE
          )
          graphics::abline(h = 0, lwd = 1, col = "gray")
          graphics::grid(NA, NULL, lwd = 0.8)
          graphics::abline(
            v = visualizeVariables()$date.time,
            col = "lightgray",
            lty = 3,
            lwd = 0.8
          )
          graphics::points(
            visualizeVariables()$date.time,
            visualizeVariables()$data,
            type = "h",
            xlim = visualizeVariables()$date.time[input$sliderx],
            ylim = input$slidery,
            col = input$integer,
            lwd = linesize
          )
          graphics::axis(
            side = 1,
            at = visualizeVariables()$date.time[dum_tick],
            labels = date.lab,
            tck = -0.025,
            col.ticks = "gray20",
            col.axis = "gray20"
          )
          graphics::axis(
            side = 1,
            at = visualizeVariables()$date.time[dum_tick],
            labels = FALSE,
            tck = 0.015,
            col.ticks = "gray20",
            col.axis = "gray20"
          )
          graphics::rug(
            x = visualizeVariables()$date.time[dum_tick2],
            ticksize = 0.015,
            side = 1,
            quiet = TRUE
          )
          graphics::axis(
            side = 2,
            tck = -0.025,
            col.ticks = "gray20",
            col.axis = "gray20"
          )
          graphics::axis(
            side = 2,
            tck = 0.015,
            col.ticks = "gray20",
            col.axis = "gray20",
            labels = FALSE
          )
          graphics::box(col = "gray20", lwd = 1)
          if (as.logical(input$trend)) {
            # calculate the linear trend
            x <-
              c(seq_along(visualizeVariables()$date.time[input$sliderx[1]:input$sliderx[2]]))
            model <-
              stats::lm(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]] ~ x, na.action = stats::na.exclude)
            graphics::lines(visualizeVariables()$date.time[input$sliderx[1]:input$sliderx[2]],
                            stats::predict(model),
                            col = "gray20",
                            lwd = linesize)
            conf <- stats::confint(model, "x", level = 0.95)
            trend <- model$coeff["x"] * 12.
            lconf <- conf[1] * 12.
            uconf <- conf[2] * 12

            mean.out <-
              format(mean(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]], na.rm = TRUE),
                     digits = 3,
                     nsmall = 1)
            trend.out <-
              paste0(
                "[",
                format(lconf, digits = 2, nsmall = 2),
                ",",
                format(trend, digits = 2, nsmall = 2),
                ",",
                format(uconf, digits = 2, nsmall = 2),
                "] "
              )
            xtext <- as.Date(visualizeVariables()$date.time[input$sliderx[1]])

            pmin <-
              min(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]], na.rm = TRUE)
            pmax <-
              max(visualizeVariables()$data[input$sliderx[1]:input$sliderx[2]], na.rm = TRUE)
            drange <- pmax - pmin
            graphics::text(
              xtext,
              pmax - 0.01 * drange,
              paste("mean:", mean.out, visualizeVariables()$unit, sep = " "),
              pos = 4,
              cex = textsize
            )
            graphics::text(
              xtext,
              pmax - 0.08 * drange,
              paste0("linear trend:", trend.out, visualizeVariables()$unit, "/yr"),
              pos = 4,
              cex = textsize
            )
          }
          graphics::mtext(input$text2_1d)
          on.exit(grDevices::dev.off())
        }
      } # end if analyze
    })
    png_path(outfile)

    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = iwidth,
      height = iheight,
      alt = "This is alternate text"
    )
  })

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
      lon_bounds( input$slider1 )
      lat_bounds( input$slider2 )
    } else {
      brush <- input$zoom_brush
      lon <- c(brush$xmin, brush$xmax)
      lat <- c(brush$ymin, brush$ymax)
      lon_bounds( lon )
      lat_bounds( lat )
    }
  })

  # Download the created png plot.
  output$downloadPlot <- downloadHandler(
    filename = function() {
      if (visualizeVariables()$plot_dim == 2) {
        return(paste0("plot_", input$text1, sessionName, ".png"))
      } else {
        return(filename = paste0("plot_", input$text1_1d, sessionName, ".png"))
      }
    },
    content = function(file) {
      # TODO: NOT SURE IF THIS WILL WORK IN REMOTE HOST. PLEASE TRY ON SHADOW!!

      file.copy(png_path(), file)
    },
    contentType = "image/png")

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

    da_mean   <- round(mean(dastat,na.rm = TRUE), digits = dg)
    da_median <- round(stats::median(dastat,na.rm = TRUE), digits = dg)
    da_sd     <- round(stats::sd(dastat,na.rm = TRUE), digits = dg)
    da_max    <- round(max(dastat,na.rm = TRUE), digits = dg)
    da_min    <- round(min(dastat,na.rm = TRUE), digits = dg)

    # some numbers
    output$statistics <- renderPrint({
      cat(paste0("Mean:               ",da_mean),"\n")
      cat(paste0("Median:             ",da_median),"\n")
      cat(paste0("Standard deviation: ",da_sd),"\n")
      cat(paste0("Maximum:            ",da_max),"\n")
      cat(paste0("Minimum:            ",da_min),"\n")
      cat(paste0("Unit:               ",visualizeVariables()$unit),"\n")
    })

    # Missing values can be found in global.R
    # histogram
    output$myHist <- renderPlot({
      dastat <- as.numeric(dastat)

      # Provide that not all values are NA
      min_max <- range(dastat)
      req(min_max[1])
      req(min_max[2])

      graphics::hist(dastat, main = paste0("Histogram of ", input$text1),
                     xlab = input$text3, col = grDevices::rgb(91, 127, 149, maxColorValue = 255))
      graphics::rect(graphics::par("usr")[1],
                     graphics::par("usr")[3],
                     graphics::par("usr")[2],
                     graphics::par("usr")[4],
                     col = "light grey")
      graphics::grid(NULL, NULL, lty = 3, col = grid_col, lwd = 1.5)
      graphics::hist(dastat, col = grDevices::rgb(91, 127, 149, maxColorValue = 255), add = TRUE)
      graphics::box(col = bordercolor, lwd = linesize)
    })
  })

  # Observing changes to instat file
  observeEvent({
    instat_path_action()
    input$plot_rinstat
  }, {
    # Requirements
    req(instat_path())
    if (input$plot_rinstat && file.exists( instat_path())) {
      # grid_col, bordercolor, and linesize can be found in global.R
      output$myComp <- renderPlot({
        lo <- as.numeric(co.data()$lon_station)
        la <- as.numeric(co.data()$lat_station)
        st <- co.data()$data_station
        sa <- co.data()$data_sat
        st <- st[order(la)]
        sa <- sa[order(la)]
        lo <- lo[order(la)]
        la <- la[order(la)]
        xlabs <- NULL
        for (i in seq_along(st)) {
          dummy <- paste0("[", round(lo[i], digits = 1), ";", round(la[i], digits = 1), "]")
          xlabs <- append(xlabs, dummy)
        }
        rd <- rbind(st, sa)
        rownames(rd) <- c("R-Instat data", "Your data")
        graphics::par(mar = c(6, 5, 3, 2))
        graphics::barplot(rd,
                          beside = TRUE,
                          main = paste0("Comparison of ", input$text1),
                          ylab = input$text3,
                          names.arg = xlabs,
                          col = c(grDevices::rgb(0, 32, 91, maxColorValue = 255),
                                  grDevices::rgb(242, 169, 0, maxColorValue = 255)),
                          las = 2)
        graphics::rect(graphics::par("usr")[1],
                       graphics::par("usr")[3],
                       graphics::par("usr")[2],
                       graphics::par("usr")[4],
                       col = "light grey")
        graphics::grid(NULL,
                       NULL,
                       lty = 3,
                       col = grid_col,
                       lwd = 1.5)
        graphics::barplot(rd,
                          beside = TRUE,
                          ylab = input$text3,
                          names.arg = xlabs,
                          col = c(
                            grDevices::rgb(0, 32, 91, maxColorValue = 255),
                            grDevices::rgb(242, 169, 0, maxColorValue = 255)),
                          las = 2,
                          add = TRUE,
                          legend.text = c("Surface", "Satellite"))
        graphics::box(col = bordercolor, lwd = linesize)
      })
    }
  })

  # File summaries
  output$summary1 <- renderPrint({
    req(nc_path_visualize())
    cmsaf::ncinfo(nc_path_visualize())
  })

  output$summary2 <- renderPrint({
    req(nc_path_visualize())
    cmsaf::ncinfo(nc_path_visualize(), "m")
  })

  # About part
  output$about <- renderPrint({
    cat("The CMSAF Visualizer is part of the CM SAF R Toolbox.","\n")
    cat("This tool helps you to visualize 1D-timeseries and 2D-maps.","\n")
    cat("\n")
    cat("This version ('Don`t Panic') was tested with the cmsaf","\n")
    cat("R-package in version 2.0.1.","\n")
    cat("\n")
    cat("Suggestions for improvements and praise for the developers","\n")
    cat("can be send to contact.cmsaf@dwd.de.","\n")
    cat("\n")
    cat("                              - Steffen Kothe - 2019-08-08 -","\n")
    cat("\n")
    cat("\n")
  })

  # Tipps
  output$tipps <- renderPrint({
    cat("When you save a figure, please add the file extension .png !","\n")
    cat("\n")
    cat("If saving an image fails, try right-click plus 'save image as'.","\n")
    cat("\n")
    cat("The orthographic projection includes some interpolation,","\n")
    cat("which can have an impact on local pixel values!","\n")
    cat("\n")
    cat("If you swim with a friend, your chances of getting eaten","\n")
    cat("by a shark will drop by 50%.","\n")
    cat("\n")
  })

  output$link <- renderUI({
    url <- a("http://www.cmsaf.eu/tools", href = "http://www.cmsaf.eu/tools")
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
}
