#' Server Function for DrugScreenExplorer Shiny Application
#'
#' This function defines the server-side logic for the DrugScreenExplorer Shiny application.
#' It handles data processing, visualization, and user interactions for drug screening analysis.
#'
#' @param ... additional arguments
#' @return Shiny server function
#'
#' @import pheatmap
#' @import RColorBrewer
#' @import plotly
#' @import dr4pl
#' @import DT
#' @import shinyBS
#' @import Rtsne
#' @import dplyr
#' @import ggplot2
#' @import shiny
#' @import plotly
#' @import stats
#' @importFrom ggbeeswarm geom_beeswarm
#' @importFrom utils unzip
#' @importFrom tidyr spread
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom DT renderDT datatable
#' @importFrom shinyBS bsTooltip
#'
#'
.buildServer <- function(...) {

  serve <- function(input, output, session) {
    # 1. Data Preprocessing (DP)----
    # 1.1 Constants and configuration ----
    # Temp dir for file handling
    temp <- file.path(tempdir(), "DrugScreenExplorer")

    # Point to rawData dir
    rawDir <- reactive({
      paste0(temp, "/rawData")
    })

    # Extract plateAnno file
    checkPlateAnno <- reactive({

      req(input$fileInput)
      checkAnnotationFile(temp,
                          "plateAnno",
                          "plateAnno")
    })
    # Extract wellAnno file 
    checkWellAnno <- reactive({
      req(input$fileInput)
      checkAnnotationFile(temp,
                          "wellAnno",
                          "wellAnno")
    })
    # Check if drug combination data is present
    ifDrug2 <- reactive({
      req(processedScreenData())
      "drug2" %in% colnames(processedScreenData())
    })

    screenData <- reactiveVal(NULL)

    # 1.2 User Inputs ----
    # User input for manual data loading
    output$getRange <- renderUI({

      req(input$fileInput)

      tagList(tags$h4("Specify Well Range",
                      style = "font-size: 14px; font-weight: bold;"),
              div(style = "display:inline-block; width: 90px",
                  numericInput("rowTop", "Top", value = 3,
                                min = 0,
                                max = 9999,
                                step = 1)),
              div(style = "display:inline-block; width: 90px",
                  numericInput("rowBottom", "Bottom", value = 18,
                                min = 0,
                                max = 9999,
                                step = 1)),
              div(style = "display:inline-block; width: 90px",
                  numericInput("colLeft", "Left", value = 2,
                                min = 0,
                                max = 9999,
                                step = 1)),
              div(style = "display:inline-block; width: 90px",
                  numericInput("colRight", "Right", value = 9999,
                                min = 0,
                                max = 9999,
                                step = 1))
      )
    })
    output$sepText <- renderUI({
      req(input$fileInput)
      textInput("sepText",
                "Separator Character",
                value = ";")
    })
    output$commaAsDecimalCheckBox <- renderUI({
      req(input$fileInput)
      checkboxInput("commaAsDecimal",
                    "Commas is used as decimal in the input data",
                    value = FALSE)
    })

    # Normalization UI elements
    output$discardLayerInput <- renderUI({
      req(input$fileInput)
      numericInput("discardLayer",
                   "The layers of discarded wells on the edge",
                   value = 0,
                   min = 0,
                   step = 1)
    })

    output$allowNACheckBox <- renderUI({
      req(input$fileInput)
      checkboxInput("allowNA", "Allow missing values",
                    value = FALSE)
    })

    output$methodUI <- renderUI({
      req(input$fileInput)
      # if (input$normalization) {
      selectInput("method", "Methods to calculate percent inhibition",
                  choices = c('negatives', 'NPI'),
                  selected = "negatives")
      # }
    })
#---Normalization UI elemets
    #Select negative and positive wells for normalization
    output$selWells <- renderUI({
      req(processedScreenData())
      df <- processedScreenData()
      
      # Create a unique list if drug combinations
      if ("drug2" %in% colnames(processedScreenData())){
        df <- df |>
          dplyr::mutate(drug_combination = paste0(drug, "_", drug2))
        allDrugs <- unique(df$drug_combination)
      } else {
        allDrugs <- unique(processedScreenData()$drug)
      }

      # selectInput
      tagList(
        selectInput("negWell","Negative control wells: drug names",
                    choices = sort(allDrugs),
                    multiple = TRUE,
                    selected = NULL),
        selectInput("posWell", "Positive wells: drug names",
                    choices = sort(allDrugs),
                    multiple = TRUE,
                    selected = NULL)
      )
    })
    #Choices of summary statistics to calculate
    output$statisticsUI <- renderUI({
      req(processedScreenData())
      if (ifDrug2()) {
        return(NULL)  # No summary statistics for drug combinations
      }
      checkboxGroupInput(
        "chosenStats",
        "Select summary statistics to calculate.",
        c(
          "Inhibitory Concentration 50 (IC50)" = "IC50",
          "Area under the curve(AUC)" = "AUC",
          "Drug Sensitivity Score (DSS)" = "DSS"
        )
      )
    })
    # DSS type selection UI
    output$dssUI <- renderUI({
      req(input$chosenStats)
      if ("DSS" %in% input$chosenStats) {
        tagList(
        checkboxGroupInput(
          "dssType", 
          "Select DSS type",
                    choices = c("DSS1", "DSS2", "DSS3"),
                    selected = "DSS1"),
        numericInput("dssThreshold", "DSS threshold",
                     value = 10,
                     min = 0,
                     max = 100,
                     step = 1)
        )
      } 
    })

    output$span <- renderUI({
      req(screenData())
      if (input$estimateMethod == "loess"){
        tagList(
          numericInput("span", "Span parameter for loess", value = 1, min = 0.1, step = 0.1),
          bsTooltip("span", "Controls the degree of smoothing of loess")
        )
      }
    })


    # 1.3 Reactive data and calculations ----
    #If no range is supplied, set colRange to NULL, which will trigger automatic parsing.
    columnRange <- reactive({
      if(input$colRight == 9999){
        end <- findColumnCount(rawDir(), 
                          input$sepText,
                          skip = input$rowTop)#Skip to row Top  
        c(input$colLeft, end)
      } else {
        c(input$colLeft, input$colRight)
      }
    })

    #Dynamically generate UI for column selection
    output$column_selector_ui <- renderUI({
      #data must be read in first.
      req(processedScreenData())
      #Ensure that sampleID is available for selection (default)
      validate(
        need("sampleID" %in% colnames(processedScreenData()),
        "Error: sampleID column is missing from the data. 
         Please check your input files.")
      )

      choices <- setdiff(colnames(processedScreenData()),
                         c("wellID", "rowID", "colID", "value"))

      selectInput("sampleAnnotations",
                  "Select sample information to display:",
                  choices = choices,
                  selected = intersect(c("sampleID", "fileName"), choices),
                  multiple = TRUE,
                  width = "100%")
    })

    # 1.4 Event observers and logical controls ----
    # When .zip file is chosen, unzip to temp dir and check all files exist
    observe({
      req(input$fileInput) #Input file chosen
      unzip(input$fileInput$datapath, exdir = temp)
      #Check all files exist 
      fileCheck <- all(c("plateAnno", "rawData", "wellAnno") %in% list.files(temp))
      if (!fileCheck){
        misFiles <- setdiff(c("plateAnno", "rawData", "wellAnno"),
                            list.files(temp))
        warning("File missing: ", misFiles, "\n\n")
      }
    })

    # When import button is clicked, read in data and generate screenData 
    processedScreenData <- eventReactive(input$doImport, {
      #Requires a .zip file (that has been validated) to be selected. 
      req(input$fileInput)

      processedScreenData <-
        tryCatch({
                  #only accept ranges if manual ranges is selected.
                  manual_ranges <- isTRUE(input$showOptions)
                  result <- readScreen(rawDir = rawDir(),
                                       plateAnnotationFile = checkPlateAnno(),
                                       wellAnnotationFile = checkWellAnno(),
                                       sep = if (manual_ranges) {
                                        input$sepText
                                       } else {
                                        ";"
                                       },
                                       setManualRanges = manual_ranges,
                                       rowRange = if (manual_ranges) {
                                        c(input$rowTop, input$rowBottom)
                                       } else {
                                        NULL
                                       },
                                       colRange = if (manual_ranges) {
                                        columnRange()
                                       } else {
                                        NULL
                                       },
                                       commaAsDecimal = if (manual_ranges) {
                                        isTRUE(input$commaAsDecimal)
                                       } else {
                                        FALSE
                                       },
                                       negWell = c(),
                                       posWell = c(),
                                       allowNA = isTRUE(input$allowNA),
                                       normalization = FALSE)
        }, error = function(e) {
          warning(
            "readScreen failed:" ,
            conditionMessage(e),
            call. = FALSE
          )
          message <- paste0("Error: ", conditionMessage(e))
          list(result = NULL, message = message)
        })
      return(processedScreenData)
    })

    normTable <- eventReactive(input$doNorm, {
      req(processedScreenData())
      # Check all values are valid for normalization
      if (is.null(input$negWell)){
        showNotification("Need negative for normalization", type = "error")
        return(NULL)
      }
      if (is.null(input$posWell) && input$method == "NPI"){
        showNotification("Need positive for normalization", type = "error")
        return(NULL)
      }
      # if the positive well is null, convert to empty list.
      posWs <- if (is.null(input$posWell)) character(0) else input$posWell
      negWs <- input$negWell #Already checked, cannot be null.

      # With drugs known, annotate the data with wellType
      df <- addWellType(processedScreenData(),
                        negctrl = negWs,
                        posctrl = posWs,
                        single = !ifDrug2())

      showModal(modalDialog("Normalizing ... May take some time.", footer = NULL))

      tryCatch({
        #normalize
        screenData <- normalizePlate(
          screenData = df,
          method = input$method,
          discardLayer = input$discardLayer,
          bySample = FALSE
        )
        # summarize if single drug
        if (!ifDrug2()) {
          screenData <- summariseScreen(
            screenData, method = input$chosenStats,
            dssType = if (is.null(input$dssType)) "DSS2" else input$dssType,
            dssThreshold = if (is.null(input$dssThreshold)) 0.1 else input$dssThreshold
          )
        }

        screenData(screenData)

        removeModal()

        return(screenData)

      },
      error = function(e) {
        removeModal()
        showNotification(paste("Error:", e$message), type = "error")
        return(NULL)
      })

    })

    # When button is pressed, perform normalization and summarization of data
    # observeEvent(input$doNorm, {

    #   showModal(modalDialog("Normalizing ... May take some time.", footer = NULL))

    #   normalizationSucceeded <- FALSE
    #   tryCatch({
    #     screenData0 <- normalizePlate(
    #       screenData0,
    #       method = input$method,
    #       discardLayer = input$discardLayer,
    #       bySample = FALSE
    #     )

    #     if (!ifDrug2()) {
    #       screenData0 <- summariseScreen(screenData0,
    #                                      method = summaryStats())
    #     }

    #     screenData(screenData0)
    #     normalizationSucceeded <- TRUE
    #   }, error = function(e) {
    #     warningMessage(paste("Error:", e$message))
    #   })

    #   removeModal()
    #   if (!normalizationSucceeded)
    #     return()
    # })

    # output$warningMessage <- renderText({
    #   warningMessage()
    # })

    # # output normalize data
    # normData <- reactive({

    #   if(ifDrug2())
    #     addDurgCol <- c("drug", "drug2")
    #   else
    #     addDurgCol <- "drug"

    #   req(screenData())
    #   selRows <- input$sampleInfo_row_last_clicked
    #   selectedPlates <- as.character(infoTableRaw()[selRows, ] %>%
    #                                    pull(fileName))

    #   if(length(selectedPlates) == 0)
    #     selectedPlates <- infoTableRaw() %>% pull(fileName)

    #   selectedPlates <- unique(selectedPlates)

    #   normTable <- screenData() %>%
    #     filter(fileName %in% selectedPlates) %>%
    #     #Adding WellLabel column to DF (required for report) FH
    #     mutate(
    #       .drug_combination = if(ifDrug2()) paste0(drug, "_", drug2) else drug,
    #       wellType = case_when(
    #         .drug_combination %in% input$negWell ~ "neg",
    #         .drug_combination %in% input$posWell ~ "pos",
    #         TRUE                                 ~ "sample"
    #       )
    #     ) %>%
    #     dplyr::select(any_of(c("sampleID", "fileName", "wellID", "rowID", "colID",
    #                            "wellType", addDurgCol, input$sampleAnnotations_norm, "value",
    #                            "normVal", "normVal.cor")))
    #   return(normTable)
    # })


    # 1.5 Outputs and Plots ----
    # Select metadata columns based on sampleAnnotation input
    infoTableRaw <- reactive({

      req(processedScreenData())

      selectedCols <- c("sampleID", input$sampleAnnotations)

      infoTable <- processedScreenData() |>
        # Select only the columns specified by the user in the UI
        dplyr::select(dplyr::all_of(selectedCols)) |>
        distinct(sampleID, .keep_all = TRUE)

      return(infoTable)
    })

    # Render sample information table
    output$sampleInfo <- DT::renderDT({
      req(infoTableRaw())
      infoTableRaw() |>
        mutate(across(where(is.character), as.factor)) |>
        DT::datatable(filter = "top",
                      rownames = FALSE,
                      selection = "single",
                      style = "bootstrap")

    })

    # Render header information for sample information table
    output$sampleInfo_ui <- renderUI({

      req(infoTableRaw())
      div(class = "table-container table1",
          h3("Sample Overview"),
          DT::DTOutput("sampleInfo")
      )
    })

    #Render header information for normalized data table
    output$normTable_ui <- renderUI({
      req(normTable())
      div(class = "table-container table2",
          h3("Normalized Values"),
          DT::DTOutput("normTable")
      )
    })

    # Render normalized data talble.
    output$normTable <- DT::renderDT({
      req(normTable())

      DT::datatable(
        normTable(),
        filter = "top",
        rownames = FALSE,
        style = "bootstrap",
      )
    })

    # 1.6 Warnings, notifications, and messages ----

    output$pathDisplay <- renderText({
      paste0("Find example data at ",
             system.file(package = "DrugScreenExplorer"), "/extdata")
    })


    output$readScreen_message <- renderText({
      req(processedScreenData())
      if (!is.null(processedScreenData()$message))
        processedScreenData()$message
    })

    ## 1.2.3 Generate QC report
    # output$reportDownload <- renderUI({
    #   #Only display option after the data has been normalized
    #   req(normData())
    #   downloadButton("downloadQC", "Download QC Report")
    # })
    # output$authorName_ui <- renderUI({
    #   req(normData())
    #   textInput("authorName", "Your Name (optional)", value = "")
    # })

    # # Make QC report
    # output$downloadQC <- downloadHandler(
    #   filename = function () {
    #     "QC_report.html"
    #   },
    #   content = function(file) {
    #     tmpdir <- tempfile("drug-screen-report-")
    #     dir.create(tmpdir, recursive = TRUE)
    #     on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

    #     norm_data <- normData()
    #     report_file <- makeReport(
    #       screenData = norm_data,
    #       dir = tmpdir,
    #       author = ifelse(is.null(input$authorName) || input$authorName == "", "Author Name", input$authorName)
    #     )

    #     copied <- file.copy(
    #       from = report_file,
    #       to = file,
    #       overwrite = TRUE
    #     )
    #     if (!isTRUE(copied)) {
    #       stop("The generated QC report could not be copied to the download path")
    #     }
    #   }
    # )

    #-----------------------------------------------------------------------------
    # 1.3 adjust incubation effect
    # Placement should be adjusted?? -FH
    # Span controls



    output$useNegControls <- renderUI({
      #Must have screen Data to load drug names
      req(screenData())

      #Extract relevant parameters for population
      conc_df <- screenData()
      max_conc <- max(conc_df$concentration, na.rm = TRUE)
      if (ifDrug2()) {
        # All drug names
        drug_names <- unique(c(conc_df$drug, conc_df$drug2))
      } else {
        drug_names <- unique(conc_df$drug)
      }

      if (!isTRUE(input$useNeg)) {
        tagList(
          numericInput("useLowConcentrations",
                       "Threshold for lowest concetrations to include in estimation",
                       value = 0,
                       min = 0,
                       max = max_conc),
          selectInput("exclude", 
                      "Drugs to exclude from edge effect estimation",
                      choices = drug_names,
                      multiple = TRUE)
        )
      }
    })

    screenData_fitted <- reactiveVal(NULL)
    correctEdgeEffectError <- reactiveVal(NULL)

    output$correctEdgeEffect_message <- renderText({
      error <- correctEdgeEffectError()

      if (!is.null(error)) {
        paste("Error:", error)
      }
    })

    observeEvent(input$doCorrEdge, {

      req(screenData())

      span <- if (is.null(input$span)) 1 else input$span
      conc_threshold <- if (is.null(input$useLowConcentrations)) 0 else input$useLowConcentrations
      exclude <- if (is.null(input$exclude)) c() else input$exclude

      showModal(modalDialog("Adjusting and summarizing ...", footer = NULL))
      on.exit(removeModal(), add = TRUE)

      correctEdgeEffectError(NULL)

      fitted <- tryCatch({
        correctEdgeEffect(screenData(),
                          estimateMethod = input$estimateMethod,
                          useNeg = input$useNeg,
                          span = span,
                          correctMethod = input$correctMethod,
                          exclude = exclude,
                          useLowConcentrations = conc_threshold)
      }, error = function(e) {
        correctEdgeEffectError(conditionMessage(e))
        NULL
      })

      req(!is.null(fitted))

      # resummarize
      # remove previous values

      rmCols <- c("meanViab.cor", "AUC.cor", "UpperLimit.cor", "IC50.cor",
                  "Slope.cor", "LowerLimit.cor")

      fitted <- fitted[, !names(fitted) %in% rmCols, drop = FALSE]

      fitted <- summariseScreen(fitted,
                                 method = c("average", "AUC", "IC50", "DSS"),
                                 dssType = "all")
      screenData_fitted(fitted)
      screenData(fitted)

    })

    # Dynamically generate UI for column selection
    output$column_selector_normTable_ui <- renderUI({

      req(screenData())
      if(ifDrug2())
        addDurgCol <- c("drug", "drug2")
      else
        addDurgCol <- "drug"

      choices <- setdiff(colnames(screenData()),
                         c("sampleID", "fileName", "wellID", "rowID", "colID",
                           addDurgCol, "value", "normVal", "normVal.cor"))
      selectInput("sampleAnnotations_norm",
                  "Select well information to display:",
                  choices = choices,
                  multiple = TRUE,
                  width = "100%")
    })


    ##############################################################################
    # Hide Table
    ##############################################################################
    # # Reactive object for getting filtered samples

    observe({
      if (ifDrug2()) {
        hideTab(inputId = "tabs", target = "Dose-response curves")
        hideTab(inputId = "tabs", target = "Clustering")
        hideTab(inputId = "tabs", target = "Association test")
        hideTab(inputId = "tabs", target = "Drug Effect Ranking")
        showTab(inputId = "tabs", target = "Plate plot")
        showTab(inputId = "tabs", target = "Overview of Two-Drug Combination")
        showTab(inputId = "tabs", target = "Drug Combination Effects")
      } else {
        hideTab(inputId = "tabs", target = "Overview of Two-Drug Combination")
        hideTab(inputId = "tabs", target = "Drug Combination Effects")
        showTab(inputId = "tabs", target = "Dose-response curves")
        showTab(inputId = "tabs", target = "Plate plot")
        showTab(inputId = "tabs", target = "Clustering")
        showTab(inputId = "tabs", target = "Association test")
        showTab(inputId = "tabs", target = "Drug Effect Ranking")
      }
    })


    ##############################################################################
    ## Second panel: plate heatmap for a plate
    ##############################################################################

    # filterData
    filteredData <- reactive({
      req(infoTableRaw())
      selRows <- input$sampleInfo_rows_all
      selectedPlates <- as.character(infoTableRaw()[selRows, ] %>%
                                       pull(fileName))
      dataNew <- filter(screenData(), fileName %in% selectedPlates)
      dataNew
    })

    # patients selection
    output$patientSeleBox <- renderUI({
      req(filteredData())
      if ("patientID" %in% colnames(filteredData())) {
        selectInput("selePatient", "Select a patient",
                    sort(unique(filteredData()$patientID)),
                    size = 5,
                    selectize = FALSE)
      }
    })

    # sample selection
    output$sampleSeleBox <- renderUI({

      req(input$selePatient)
      if (all(c("patientID", "sampleID") %in% colnames(filteredData()))) {
        sampleIDs <- unique(filter(filteredData(),
                                   patientID == input$selePatient)$sampleID)
      } else if ("sampleID" %in% colnames(filteredData())) {
        sampleIDs <- unique(filteredData()$sampleID)
      } else
        sampleIDs <- NULL

      if (!is.null(sampleIDs)) {
        selectInput("seleSample",
                    "Select a sample",
                    sort(sampleIDs),
                    size = 3,
                    selectize = FALSE)
      }
    })

    # plate selection
    output$plateSeleBox <- renderUI({

      req(input$seleSample)
      if ("sampleID" %in% colnames(filteredData())) {
        plateIDs <- unique(filter(filteredData(),
                                  sampleID == input$seleSample)$fileName)
      } else {
        plateIDs <- unique(filteredData()$fileName)
      }
      selectInput("selePlate",
                  "Select a plate file",
                  sort(plateIDs),
                  size = 3,
                  selectize = FALSE)
    })

    # value type selection
    output$valueSeleRadio <- renderUI({

      if ("normVal.cor" %in%  colnames(filteredData())){
        opts <- c("viability", "Adjusted viability", "z-score", "raw count")
      } else if ("normVal" %in% colnames(filteredData())) {
        opts <- c("viability", "z-score", "raw count")
      } else {
        opts <- c("z-score", "raw count")
      }

      radioButtons("ifNorm", "Select value type", opts)
    })

    output$ifCensorCheck <- renderUI({
      if (!is.null(input$ifNorm) &&
          input$ifNorm %in% c("viability", "Adjusted viability", "z-score")) {
        checkboxInput("ifCensor", "Fix value range", value = FALSE)
      }
    })

    output$censorLimitBox <- renderUI({
      lower <- NULL
      upper <- NULL

      if (!is.null(input$ifCensor) && input$ifCensor) {
        if (input$ifNorm %in% c("viability", "Adjusted viability")) {
          upper <- 1.5
          lower <- 0
        } else if (input$ifNorm == "z-score") {
          upper <- 4
          lower <- -4
        }
      }

      tagList(div(style = "display:inline-block; width: 90px",
                  textInput("lowerLimit", "lower limit", value = lower)),
              div(style = "display:inline-block; width: 90px",
                  textInput("upperLimit", "upper limit", value = upper)))
    })

    plotTab <- reactive({

      req(input$selePlate) #requires plate to be selected
      ## Function to generate an ordered row IDs based on the number of rows
      genRowIDs <- function(x) {
        c(paste0(LETTERS, 0),
          paste0(rep(LETTERS, each = 26),
                 rep(LETTERS, times = 26)))[seq(1, x)]
      }

      ## Function to generate an ordered column IDs based on the number of columns
      genColIDs <- function(x) {
        sprintf("%02d", seq(x))
      }

      nCol <- length(unique(filteredData()$colID))
      nRow <- length(unique(filteredData()$rowID))
      rowSeq <- genRowIDs(nRow)
      colSeq <- genColIDs(nCol)
      matPlate <- filter(filteredData(), fileName == input$selePlate) %>%
        mutate(rowID = factor(rowID, levels = rev(rowSeq))) %>%
        mutate(colID = factor(colID, levels = colSeq))

      if (input$ifNorm == "viability") {
        matPlate <- mutate(matPlate, val = normVal)
      } else if (input$ifNorm == "Adjusted viability"){
        matPlate <- mutate(matPlate, val = normVal.cor)
      } else if (input$ifNorm == "raw count") {
        matPlate <- mutate(matPlate, val = value)
      } else if (input$ifNorm == "z-score") {
        matPlate <- mutate(matPlate, val = (value - mean(value)) / sd(value))
      }

      if (!is.null(input$ifCensor) && input$ifCensor) {
        matPlate <-
          mutate(matPlate,
                 val = if_else(val > as.numeric(input$upperLimit),
                               as.numeric(input$upperLimit), val)) %>%
          mutate(val = if_else(val < as.numeric(input$lowerLimit),
                               as.numeric(input$lowerLimit), val))
      }
      matPlate
    })

    # Plot the edge effect plot for the selected plate
    edgeEffectMatrix <- reactive ({
      #A plate must be selected by the user
      req(input$selePlate)

      #Adjust dataframe to select only values for said plate
      df <- filteredData() %>%
        filter(fileName == input$selePlate)
      
      span <- if (is.null(input$span)) 1 else input$span
      low_conc <- if (is.null(input$useLowConcentrations)) 0L else input$useLowConcentrations
      excluded <- if (is.null(input$exclude)) c() else input$exclude

      df <- fitEdgeEffect(df,
        method = input$estimateMethod,
        useNeg = input$useNeg,
        span = span,
        useLowConcentrations = low_conc,
        exclude = excluded
      )
      
      df
    })

    output$edgeEffectPlot <- renderPlotly({
      req(edgeEffectMatrix())

      df <- edgeEffectMatrix()
      p <- plotPlate(df, plotType = "edgeEffect")

      ggplotly(p[[1]])
    })


    # plot the heatmap
    output$plateHeatmap <- renderPlotly({

      req(input$selePlate)
      p <- ggplot(plotTab(),
                  aes(x = colID, y = rowID, fill = val)) +
        geom_tile(color = "gray", size = 3) +
        scale_fill_viridis_c() +
        theme_minimal() +
        theme(axis.text = element_text(size = 6),
              axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
              plot.margin = margin(5, 5, 5, 5)) +
        labs(x = "", y = "", title = input$selePlate)


      if (input$ifNorm %in% c("viability", "Adjusted viability", "z-score")) {

        if (!is.null(input$ifCensor) && input$ifCensor) {
          p <- p + scale_fill_gradient2(high = "red",
                                        low = "blue",
                                        mid = "white",
                                        midpoint = 1,
                                        limits = c(as.numeric(input$lowerLimit),
                                                   as.numeric(input$upperLimit)),
                                        name = input$ifNorm)
        } else {
          p <- p + scale_fill_gradient2(high = "red",
                                        low = "blue",
                                        mid = "white",
                                        midpoint = 1,
                                        name = input$ifNorm)
        }
      } else if (input$ifNorm == "raw count") {
        p <- p + scale_fill_gradient(low = "blue",
                                     high = "red",
                                     name = input$ifNorm)
      }

      ggplotly(p) %>%
        config(displayModeBar = FALSE)
    })

    ## a table for selected plate and well information
    output$infoTab <- DT::renderDT({

      s <- event_data("plotly_click")
      # Add potential interesting columns that may or may not exist

      addCol <- intersect(c("drug", "drug2", "wellType", "concentration",
                            "concentration2"), colnames(plotTab()))

      showAnno <- unique(c(input$sampleAnnotations, addCol))
      showAnno <- showAnno[showAnno != "fileName"]
      infoTab  <- filter(plotTab(),
                         rowID == levels(plotTab()$rowID)[s$y],
                         colID == levels(plotTab()$colID)[s$x]) %>%
        select(!!showAnno) %>%
        mutate(`plate median value` = median(plotTab()$value)) %>%
        t()

      datatable(infoTab,
                colnames = "",
                selection = "none",
                options = list(dom = "t"))
    })

    # prepare table for the dose-response plot on the first panel
    linePlotTab1 <- reactive({

      if (any(!c("drug", "concentration") %in% colnames(plotTab()))) {
        NULL
      } else {
        s <- event_data("plotly_click")
        drugName <- unique(filter(plotTab(),
                                  rowID == levels(plotTab()$rowID)[s$y],
                                  colID == levels(plotTab()$colID)[s$x])$drug)
        plotTab <- filter(plotTab(), drug == drugName)
        plotTab
      }
    })

    # dose-response curve for selected drugs if possible.
    output$detailPlot <- renderPlotly({

      if (any(!c("drug", "concentration") %in% colnames(plotTab()))) {
        text <- "drug name, concentration and normalized viability\n must
      be available in order to show\n dose-response curve"

        p <- ggplot() +
          annotate("text", x = 4, y = 25, size = 3, label = text, col = "red") +
          theme_void() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())

      } else {

        s <- event_data("plotly_click")

        if (!ifDrug2()) {
          drugName <- unique(filter(plotTab(),
                                    rowID == levels(plotTab()$rowID)[s$y],
                                    colID == levels(plotTab()$colID)[s$x])$drug)

          plotTab <- filter(plotTab(), drug == drugName)
          p <- ggplot(plotTab, aes(x = concentration, y = val)) +
            geom_point(col = "red") +
            geom_line(col = "red") +
            scale_x_log10() +
            theme_bw() +
            ylab(input$ifNorm) +
            ggtitle(drugName) +
            theme(plot.title = element_text(hjust = 0.5,
                                            face = "bold",
                                            color = "red"),
                  axis.text  = element_text(size = 13),
                  axis.title = element_text(size = 13))
        } else {

          drugName <- unique(filter(plotTab(),
                                    rowID == levels(plotTab()$rowID)[s$y],
                                    colID == levels(plotTab()$colID)[s$x])$drug)

          drugName2 <- unique(filter(plotTab(),
                                     rowID == levels(plotTab()$rowID)[s$y],
                                     colID == levels(plotTab()$colID)[s$x])$drug2)

          plotTab <- filter(plotTab(), drug == drugName, drug2 == drugName2) %>%
            group_by(concentration, concentration2) %>%
            summarise(val = mean(val))


          p <- ggplot(plotTab,
                      aes(x = as.factor(concentration2),
                          y = as.factor(concentration),
                          fill = val)) +
            geom_tile(color = "gray") +
            # scale_fill_viridis_c() +
            labs(x = "Concentration2", y = "Concentration", fill = "Value") +
            theme_minimal() +
            theme(axis.text = element_text(size = 6),
                  axis.ticks = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                  plot.margin = margin(5, 5, 5, 5))

          if (input$ifNorm %in% c("viability", "Adjusted viability", "z-score")) {
            if (!is.null(input$ifCensor) && input$ifCensor) {
              p <- p + scale_fill_gradient2(high = "red",
                                            low = "blue",
                                            mid = "white",
                                            midpoint = 1,
                                            limits = c(as.numeric(input$lowerLimit),
                                                       as.numeric(input$upperLimit)),
                                            name = input$ifNorm)
            } else {
              p <- p + scale_fill_gradient2(high = "red",
                                            low = "blue",
                                            mid = "white",
                                            midpoint = 1,
                                            name = input$ifNorm)
            }
          } else if (input$ifNorm == "raw count") {
            p <- p + scale_fill_gradient(low = "blue",
                                         high = "red",
                                         name = input$ifNorm)
          }
        }
      }
      ggplotly(p) %>%
        config(displayModeBar = FALSE)
    })

    ##############################################################################
    ## Third panel: dose-response curves
    ##############################################################################

    ## select patient group for subsetting
    output$seleGroupBox <- renderUI({
      sampleAnnotations <- input$sampleAnnotations
      selectInput("selectGroup",
                  "Subset samples by",
                  c("All samples", sampleAnnotations),
                  selected = "All samples")
    })

    # select a drug name
    output$nameDrugBox <- renderUI({
      req(filteredData())
      allDrugs <- filter(filteredData(), wellType == "sample") %>%
        distinct(drug) %>%
        pull(drug)
      selectInput("nameDrug", "Select a drug", allDrugs)
    })

    ## select features for grouping of the patients
    output$seleFeatureBox <- renderUI({
      req(input$selectGroup)
      if (!is.null(input$selectGroup) && input$selectGroup != "All samples") {
        allGroups <- unique(pull(filteredData(), input$selectGroup))
        selectInput("seleSub", "Groups included",
                    allGroups,
                    size = 5,
                    selectize = FALSE,
                    multiple = TRUE,
                    selected = allGroups[1])
      }
    })

    colClass <- reactive({

      if (!is.null(input$sampleAnnotations))
        sampleAnnotations <- c("sampleID", "fileName", input$sampleAnnotations)
      else
        sampleAnnotations <- c("sampleID", "fileName")

      colRes <- sapply(sampleAnnotations,
                       function(n) {
                         detectClass(
                           filteredData() %>%
                             as.data.frame() %>%
                             dplyr::distinct(sampleID, .keep_all = TRUE) %>%
                             dplyr::select(all_of(n)) %>%
                             pull())
                       })
      return(colRes)
    })

    ## select features for color scheme
    output$colorBox <- renderUI({
      if (!is.null(input$sampleAnnotations))
        sampleAnnotations <- c("sampleID", "fileName", input$sampleAnnotations)
      else
        sampleAnnotations <- c("sampleID", "fileName")

      sampleAnnotations <- c("sampleID", "fileName",
                             sampleAnnotations[colClass() != "nd"]) # remove NA
      selectInput("seleColor",
                  "Color by",
                  sampleAnnotations,
                  size = 5,
                  selectize = FALSE,
                  multiple = FALSE,
                  selected = sampleAnnotations[1])
    })

    # show option for incubation effect correction if 'normVal.cor' is present
    output$ifCorCheck1 <- renderUI({
      if ("normVal.cor" %in% colnames(filteredData())) {
        checkboxInput("ifCorrected1",
                      "Incubation effect correction",
                      value = FALSE)
      }
    })

    # data table for dose-response plot
    doseTab <- reactive({
      req(input$nameDrug)
      if (input$selectGroup == "All samples") {
        plotTab <- filter(filteredData(), drug == input$nameDrug)
      } else {
        selePlates <- filteredData() %>%
          filter(!!sym(input$selectGroup) %in% input$seleSub) %>%
          pull(fileName) %>%
          unique()

        plotTab <- filter(filteredData(),
                          fileName %in% selePlates,
                          drug == input$nameDrug)
      }
      plotTab
    })

    # reactive object to make the dose-response curves
    doseCurves <- reactive({

      req(doseTab())
      fmt_dcimals <- function(decimals = 0) {
        # return a function responpsible for formatting the axis labels with a
        # given number of decimals
        function(x) as.character(round(x, decimals))
      }

      plotTab <- doseTab()

      colType <- colClass()[input$seleColor]
      if (colType != "continuous")
        plotTab[[input$seleColor]] <- as.factor(plotTab[[input$seleColor]])

      # ifCorrected
      if (!is.null(input$ifCorrected1) && input$ifCorrected1) {
        valueType <- "normVal.cor"
      } else {
        valueType <- "normVal"
      }

      p <- ggplot(data = plotTab,
                  aes_string(x = "concentration",
                             y = valueType,
                             col = input$seleColor,
                             group = "sampleID")) +
        geom_point(pch = 16, size = 3) +
        theme_classic() +
        scale_x_log10(labels = fmt_dcimals(3)) +
        theme(axis.line.x = element_line(linewidth = 0.5),
              plot.title = element_text(
                hjust = 0.5,
                face = "bold",
                color = "red")) +
        ylab("Viability") +
        xlab("Concentration") +
        # coord_cartesian(ylim = c(0, 1.5)) +
        ggtitle(as.character(input$nameDrug))

      if (input$ifIC50){
        p <- p + geom_smooth(method = "fitIC50",
                             se = FALSE,
                             method.args = list(logDose = 10))
      } else {
        p <- p + stat_summary(aes(group = sampleID),
                              fun.y = mean,
                              geom = "line")
      }

      if (colType == "continuous") {
        p <- p + scale_color_gradient(low = "blue", high = "red")
      }
      return(p)
    })

    output$curvePlot <- renderPlotly({
      ggplotly(doseCurves()) %>%
        config(displayModeBar = FALSE)
    })

    ##############################################################################
    ## Fourth panel: Clustering plot
    ##############################################################################

    # select patient group for subsetting
    output$seleGroup1 <- renderUI({
      selectInput("selectGroup1",
                  "Subset samples by",
                  c("All samples", input$sampleAnnotations),
                  multiple = FALSE,
                  selected = "All samples")
    })

    # select the summarisation statistics
    output$sumTypeBox <- renderUI({
      typeMap <- structure(c("meanViab", "AUC", "IC50", "DSS",
                             "DSS1", "DSS2", "DSS3"),
                           names = c("average viability", "AUC", "IC50",
                                     "DSS", "DSS1", "DSS2", "DSS3"))
      typeAvail <- typeMap[typeMap %in% colnames(filteredData())]
      radioButtons("seleSumType", "Drug effect summarisation method", typeAvail)
    })

    # select features for grouping of the patients
    output$seleFeature1 <- renderUI({
      if (!is.null(input$selectGroup1) && input$selectGroup1 != "All samples") {
        # if (input$selectGroup1 != "All samples") {
        allGroups <- unique(pull(filteredData(), input$selectGroup1))
        selectInput("seleSub1",
                    "Groups included",
                    allGroups,
                    size = 5,
                    selectize = FALSE,
                    multiple = TRUE,
                    selected = allGroups[1])
      }
    })

    # select features for color scheme on the fourth pannel (for clustering plot)
    output$colorBox1 <- renderUI({

      if (!is.null(input$sampleAnnotations))
        sampleAnnotations <- c("sampleID", "fileName", input$sampleAnnotations)
      else
        sampleAnnotations <- c("sampleID", "fileName")

      sampleAnnotations <- c("sampleID", "fileName",
                             sampleAnnotations[colClass() != "nd"])
      selectInput("seleColor1", "Color by",
                  sampleAnnotations,
                  size = 5,
                  selectize = FALSE,
                  multiple = FALSE,
                  selected = sampleAnnotations[1])
    })

    # show option for incubation effect correction if 'normVal.cor' is present
    output$ifCorCheck2 <- renderUI({
      if ("normVal.cor" %in% colnames(filteredData())) {
        checkboxInput("ifCorrected2", "Incubation effect correction", value = FALSE)
      }
    })

    output$censorLimitBox1 <- renderUI({
      if (!is.null(input$ifCensor1) && input$ifCensor1) {
        upper <- 1.5
        lower <- 0
        tagList(div(style = "display:inline-block; width: 90px",
                    textInput("upperLimit1", "upper limit", value = upper)),
                div(style = "display:inline-block; width: 90px",
                    textInput("lowerLimit1", "lower limit", value = lower)))
      }
    })

    # table for the clustering plot on panel 3
    plotMat <- reactive({

      if (is.null(input$selectGroup1) | input$selectGroup1 == "All samples") {
        plotTab <- filter(filteredData(), wellType == "sample")
      } else {
        selePlates <- filteredData() %>%
          filter(!!sym(input$selectGroup1) %in% input$seleSub1) %>%
          pull(fileName)
        plotTab <- filter(filteredData(), fileName %in% selePlates, wellType == "sample")
      }

      plotTab <- mutate(plotTab,
                        viab = if (!is.null(input$ifCorrected2) && input$ifCorrected2)
                          plotTab[[paste0(input$seleSumType, ".cor")]]
                        else
                          plotTab[[input$seleSumType]])

      if (input$ifCensor1) {
        plotTab <- plotTab %>%
          mutate(viab = ifelse(viab > as.numeric(input$upperLimit1),
                               as.numeric(input$upperLimit1), viab)) %>%
          mutate(viab = ifelse(viab < as.numeric(input$lowerLimit1),
                               as.numeric(input$lowerLimit1), viab))
      }

      viabMat <- group_by(plotTab, drug, sampleID) %>%
        summarise(viab = mean(viab))

      # idMap <- list(origin = as.character(viabMat$sampleID))
      idMap <- tibble(origin = as.character(unique(viabMat$sampleID)))

      viabMat <- spread(viabMat, key = sampleID, value = viab) %>%
        data.frame() %>%
        column_to_rownames("drug") %>%
        as.matrix()

      # in case the column names begins with number and changed by R
      idMap$new <- colnames(viabMat)
      viabMat <- viabMat[complete.cases(viabMat), ]
      #only use drugs that present in all samples

      # variant filtering
      sds <- apply(viabMat, 1, sd)
      viabMat <- viabMat[sds >= quantile(sds, 1 - input$topPercent/100), ]

      # CRITICAL FIX: Ensure the matrix is not empty before returning it
      shiny::req(nrow(viabMat) > 0)

      list(viabMat = viabMat, idMap = idMap)
    })

    # reactive object for PCA
    tabPCA <- reactive({
      viabMat <- plotMat()$viabMat
      pcaRes  <- prcomp(t(viabMat), scale = FALSE, center = TRUE)
      varExp  <- (pcaRes$sdev^2 / sum(pcaRes$sdev^2)) * 100
      pcaTab  <- data.frame(pcaRes$x[, c(1, 2)])
      names(varExp) <- colnames(pcaRes$x)
      rownames(pcaTab) <- colnames(viabMat)
      list(pcaTab = pcaTab, varExp = varExp)
    })

    # reactive object for tSNE
    tabTSNE <- reactive({

      viabMat <- plotMat()$viabMat
      # prepare distance matrix
      distViab <- dist(t(viabMat))

      tsneRes <- tryCatch({
        tsneRes <- Rtsne(distViab,  # You will need to ensure distViab is defined in your Shiny app
                         perplexity = input$perplex,
                         theta = 0,
                         max_iter = input$Niter,
                         is_distance = TRUE,
                         dims = 2)
        list(result = tsneRes, message = NULL)
      }, warning = function(w) {
        list(result = NULL, message = paste("Warning:", conditionMessage(w)))
      }, error = function(e) {
        list(result = NULL, message = paste("Error:", conditionMessage(e)))
      })

      if (!is.null(tsneRes$result)) {
        tsneRes <- tsneRes$result$Y
        rownames(tsneRes) <- labels(distViab)
        colnames(tsneRes) <- c("x", "y")
        plotTab <- data.frame(tsneRes)
        rownames(plotTab) <- colnames(viabMat)
        # print(plotTab)
        return(plotTab)
      } else {
        return(tsneRes)
      }
    })

    output$tsne_message <- renderText({
      if (input$plotType == "t-SNE")
        if (!is.null(tabTSNE()$message))
          tabTSNE()$message
    })

# Single reactive expression that handles ALL plot types and returns a ggplot
plotAll <- eventReactive(input$doPlot, {
  viabData <- filteredData()
  viabMat <- plotMat()$viabMat
  idMap <- plotMat()$idMap

  # Annotation preparation
  annoCol <- tibble(sampleID_new = colnames(viabMat),
                    sampleID = idMap[match(colnames(viabMat), idMap$new), ]$origin) %>%
    left_join(select(viabData, sampleID, all_of(input$seleColor1)), by = "sampleID") %>%
    select(-sampleID) %>%
    distinct(sampleID_new, .keep_all = TRUE) %>%
    as.data.frame() %>%
    column_to_rownames("sampleID_new")

  colType <- colClass()[input$seleColor1]

  # --- 1. HEATMAP ---
  if (input$plotType == "Heatmap") {
    hc <- hclust(as.dist(1 - cor(viabMat)), method = "ward.D2")
    hr <- hclust(as.dist(1 - cor(t(viabMat))), method = "ward.D2")

    if (input$ifRowNorm == "Row") {
      viabMat <- t(scale(t(viabMat)))
    } else if (input$ifRowNorm == "Column") {
      viabMat <- scale(viabMat)
    }

    # Reorder matrix rows and columns based on hierarchical clustering
    viabMat_ordered <- viabMat[hr$order, hc$order]

    # Convert to long format for ggplot geom_tile
    plotTab <- as.data.frame(viabMat_ordered) %>%
      rownames_to_column("RowVar") %>%
      pivot_longer(-RowVar, names_to = "ColVar", values_to = "Value")

    plotTab$RowVar <- factor(plotTab$RowVar, levels = rev(rownames(viabMat_ordered)))
    plotTab$ColVar <- factor(plotTab$ColVar, levels = colnames(viabMat_ordered))

    g <- ggplot(plotTab, aes(x = ColVar, y = RowVar, fill = Value)) +
      geom_tile() +
      scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      labs(x = "Samples", y = "Features / Drugs", fill = "Value")

  # --- 2. DRUG-DRUG CORRELATION ---
  } else if (input$plotType == "Drug-drug correlation") {
    corMat <- cor(t(viabMat), method = "spearman")
    hc_cor <- hclust(as.dist(1 - corMat), method = "ward.D2")
    corMat_ordered <- corMat[hc_cor$order, hc_cor$order]

    plotTab <- as.data.frame(corMat_ordered) %>%
      rownames_to_column("RowVar") %>%
      pivot_longer(-RowVar, names_to = "ColVar", values_to = "Value")

    plotTab$RowVar <- factor(plotTab$RowVar, levels = rev(rownames(corMat_ordered)))
    plotTab$ColVar <- factor(plotTab$ColVar, levels = colnames(corMat_ordered))

    g <- ggplot(plotTab, aes(x = ColVar, y = RowVar, fill = Value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      labs(x = "Drugs", y = "Drugs", fill = "Spearman Cor")

  # --- 3. PCA ---
  } else if (input$plotType == "PCA") {
    pcaTab <- cbind(tabPCA()$pcaTab, annoCol)
    varExp <- tabPCA()$varExp
    pcaTab$sampleID <- rownames(pcaTab)

    if (colType != "continuous")
      pcaTab[[input$seleColor1]] <- as.factor(pcaTab[[input$seleColor1]])

    g <- ggplot(pcaTab, aes(x = PC1, y = PC2, color = .data[[input$seleColor1]])) +
      geom_point(size = 3) +
      theme_bw() +
      xlab(sprintf("PC1 (%2.1f%%)", varExp[1])) +
      ylab(sprintf("PC2 (%2.1f%%)", varExp[2]))

  # --- 4. T-SNE ---
  } else if (input$plotType == "t-SNE") {
    plotTab <- cbind(tabTSNE(), annoCol)
    plotTab$sampleID <- rownames(plotTab)
    if (colType != "continuous")
      plotTab[[input$seleColor1]] <- as.factor(plotTab[[input$seleColor1]])

    g <- ggplot(plotTab, aes(x = x, y = y, color = .data[[input$seleColor1]])) + 
      geom_point(size = 3) + 
      theme_bw()
  }

  # Add continuous color gradient styling if applicable for scatter plots
  if (input$plotType %in% c("PCA", "t-SNE") && colType == "continuous") {
    g <- g + scale_color_gradient(low = "blue", high = "red")
  }

  return(g)
})

# Single renderPlotly handles everything smoothly
output$clusterPlot <- renderPlotly({
  ggplotly(plotAll())
})
    # download the cluster plot
    # output$download <- downloadHandler(
    #   filename = paste0(input$plotType, ".pdf", sep = ""),
    #
    #   content = function(file) {
    #     ggsave(file, plot = plotCluster(), device = "pdf",
    #            width = input$figWidth, height = input$figHeight, limitsize = FALSE)
    #   })


    # download the cluster plot
    output$download <- downloadHandler(
      filename = function() {
        paste0(input$plotType, ".pdf")
      },
      content = function(file) {
        pdf(file, width = input$figWidth, height = input$figHeight)
        plotCluster()
        dev.off()
      }
    )

    ##############################################################################
    # Panel 5
    ##############################################################################
    #
    #   library(ComplexHeatmap)
    #   library(circlize)
    #   library(RColorBrewer)

    # select patient group for subsetting
    output$seleGroup2 <- renderUI({
      selectInput("selectGroup2",
                  "Subset samples by",
                  c("All samples", input$sampleAnnotations),
                  selected = "All samples")
    })

    # select features for grouping of the patients
    output$seleFeature2 <- renderUI({
      req(input$selectGroup2)
      if (input$selectGroup2 != "All samples") {
        allGroups <- unique(filteredData()[[input$selectGroup2]])
        selectInput("seleSub2",
                    "Groups included",
                    allGroups,
                    size = 5,
                    selectize = FALSE,
                    multiple = TRUE,
                    selected = allGroups[1])
      }
    })

    # show option for incubation effect correction if 'normVal.cor' is present
    output$ifCorCheck3 <- renderUI({
      if ("normVal.cor" %in% colnames(filteredData())) {
        checkboxInput("ifCorrected3",
                      "Incubation effect correction",
                      value = FALSE)
      }
    })

    # show options for the summarisation method for viability
    output$sumTypeBox1 <- renderUI({
      typeMap <- structure(c("meanViab", "AUC", "IC50",
                             "DSS1", "DSS2", "DSS3"),
                           names = c("average viability", "AUC", "IC50",
                                     "DSS1", "DSS2", "DSS3"))
      typeAvail <- typeMap[typeMap %in% colnames(filteredData())]
      radioButtons("seleSumType1",
                   "Drug effect summarisation method",
                   typeAvail)
    })

    # select features for hypothesis testing
    output$factorBox <- renderUI({
      req(input$sampleAnnotations)
      sampleAnnotations <- input$sampleAnnotations[colClass()[-c(1, 2)] != "nd"]
      # -c(1, 2), because colClass() addtional has sampleID and fileName
      selectInput("seleFactor",
                  "Select feature for testing",
                  sampleAnnotations,
                  size = 5,
                  selectize = FALSE,
                  multiple = FALSE,
                  selected = sampleAnnotations[1])
    })

    # text box to show the model for testing
    output$methodBox <- renderText({
      if (!is.null(input$seleFactor)) {
        colType <- colClass()[input$seleFactor]
        typeMap <- c(continuous = "Correlation test",
                     categorical = "One-way ANOVA",
                     binary = "Student's T-test")
        paste0("Test used:", typeMap[colType])
      }
    })

    # text box to show the model for testing
    output$paramBox <- renderUI({

      if (!is.null(input$seleFactor)) {
        colType <- colClass()[input$seleFactor]
        if (colType == "categorical") {
          return(NULL)
        } else {
          if (colType == "binary") {
            optParam <- c("Equal variance", "Unequal variance")
          } else if (colType == "continuous") {
            optParam <- c("Parametric (Pearson)", "Nonparametric (Spearman)")
          }
          radioButtons("seleParam", "Choose test parameter", choices = optParam)
        }
      }
    })

    # reative object to prepare viability table for testing
    testInput <- reactive({
      # If a selection factor is chosen, subset the data accordingly
      if (!is.null(input$seleFactor)) {
        # subset the screen data according to the selection
        if (is.null(input$selectGroup2) || input$selectGroup2 == "All samples") {
          plotTab <- filter(filteredData(), wellType == "sample")
        } else if (input$selectGroup2 %in% names(filteredData()) &&
                   !is.null(input$seleSub2) && length(input$seleSub2) > 0) {
          selePlates <- filteredData() %>%
            filter(.data[[input$selectGroup2]] %in% input$seleSub2) %>%
            pull(fileName) %>%
            unique()
          plotTab <- filter(filteredData(), fileName %in% selePlates, wellType == "sample")
        } else {
          plotTab <- filter(filteredData(), wellType == "sample")
        }

        selected_summary <- input$seleSumType1
        if (isTRUE(input$ifCorrected3)) {
          selected_summary <- paste0(selected_summary, ".cor")
        }
        if (!selected_summary %in% names(plotTab)) {
          errorMsg(paste0("The selected summary column '", selected_summary,
                          "' is not available. Recalculate the summary after ",
                          "applying edge-effect correction."))
          return(plotTab[0, , drop = FALSE])
        }
        plotTab <- mutate(plotTab, viab = .data[[selected_summary]])

        colType <- colClass()[input$seleFactor]
        plotTab <- group_by(plotTab, drug, sampleID) %>%
          summarise(viab = mean(viab, na.rm = TRUE), .groups = "drop") %>%
          ungroup() %>%
          mutate(status = filteredData()[match(sampleID, filteredData()$sampleID), ][[input$seleFactor]]) %>%
          filter(!is.na(status), is.finite(viab))

        if (nrow(plotTab) == 0) {
          return(plotTab)
        }

        if (colType != "continuous")
          plotTab$status <- as.factor(as.character(plotTab$status))

        plotTab
      }
    })


    # A reactiveVal to store error messages
    errorMsg <- reactiveVal(NULL)

    testRes <- reactive({

      tryCatch({
        errorMsg(NULL)  # Reset error message

        if (!is.null(input$seleFactor) && !is.null(testInput())) {
          colType <- colClass()[input$seleFactor]
          testTabInput <- testInput()

          if (nrow(testTabInput) == 0) {
            if (is.null(errorMsg())) {
              errorMsg("No valid data remained after filtering for the selected summary and grouping.")
            }
            return(tibble())
          }

          if (colType == "binary") {
            eligible_drugs <- testTabInput %>%
              count(drug, status, name = "n") %>%
              group_by(drug) %>%
              summarise(n_groups = n(), min_n = min(n), .groups = "drop") %>%
              filter(n_groups == 2, min_n >= 2) %>%
              pull(drug)

            if (length(eligible_drugs) == 0) {
              errorMsg("No drug has at least two observations in each of two groups for a t-test.")
              return(tibble())
            }
            testTabInput <- filter(testTabInput, drug %in% eligible_drugs)

            testParam <- input$seleParam == "Equal variance"
            testTab <- testTabInput %>%
              group_by(drug) %>%
              nest()
            res <- mutate(testTab, model = map(data, ~ t.test(viab ~ status, .,
                                                   var.equal = testParam))) %>%
              mutate(out = map(model, broom::tidy)) %>%
              unnest(out) %>%
              ungroup() %>%
              mutate(diffMean = estimate2 - estimate1, adjusted.p = p.adjust(p.value, method = "BH")) %>%
              select(drug, diffMean, p.value, adjusted.p) %>%
              arrange(p.value)

          } else if (colType == "continuous") {
            testParam <- ifelse(input$seleParam != "Nonparametric (Spearman)", "pearson", "spearman")
            eligible_drugs <- testTabInput %>%
              group_by(drug) %>%
              summarise(n = n(),
                        n_status = n_distinct(status),
                        .groups = "drop") %>%
              filter(n >= 3, n_status >= 2) %>%
              pull(drug)

            if (length(eligible_drugs) == 0) {
              errorMsg("No drug has enough finite observations in at least two groups for correlation testing.")
              return(tibble())
            }
            testTabInput <- filter(testTabInput, drug %in% eligible_drugs)
            testTab <- testTabInput %>%
              group_by(drug) %>%
              nest()
            res <- mutate(testTab,
                          model = map(data, ~ cor.test(viab, status, data = .,
                                                       method = testParam))) %>%
              mutate(out = map(model, broom::tidy)) %>%
              unnest(out) %>%
              ungroup() %>%
              mutate(coefficient = estimate, adjusted.p = p.adjust(p.value, method = "BH")) %>%
              select(drug, coefficient, p.value, adjusted.p) %>%
              arrange(p.value)

          } else if (colType == "categorical") {
            eligible_drugs <- testTabInput %>%
              count(drug, status, name = "n") %>%
              group_by(drug) %>%
              summarise(n_groups = n(),
                        n_total = sum(n),
                        .groups = "drop") %>%
              filter(n_groups >= 2, n_total > n_groups) %>%
              pull(drug)

            if (length(eligible_drugs) == 0) {
              errorMsg("No drug has enough finite observations across groups for ANOVA.")
              return(tibble())
            }
            testTabInput <- filter(testTabInput, drug %in% eligible_drugs)
            testTab <- testTabInput %>%
              group_by(drug) %>%
              nest()
            res <- mutate(testTab, model = map(data, ~ aov(lm(viab ~ status, .)))) %>%
              mutate(out = map(model, broom::tidy)) %>%
              unnest(out) %>%
              ungroup() %>%
              filter(term == "status") %>%
              mutate(adjusted.p = p.adjust(p.value, method = "BH")) %>%
              select(drug, statistic, p.value, adjusted.p) %>%
              arrange(p.value)
          }

          res
        }
      }, error = function(e) {
        errorMsg(paste("Error during test calculation:", e$message))
        NULL  # return NULL if error
      })
    })

    output$errorText <- renderUI({
      req(errorMsg())
      div(style = "color: red;", errorMsg())
    })

    # render the output table
    output$testRes <- DT::renderDT({

      if (!is.null(testRes())) {
        testRes() %>%
          mutate_if(is.numeric, formatC, digits = 2, format = "e") %>%
          datatable(selection = "single", options = list(dom = "tip", pageLength = 6))
      }
    })

    # boxplot for the t-Test panel
    output$diffPlot <- renderPlotly({

      if (!is.null(testInput()) && !is.null(input$seleFactor)) {
        plotTab <- testInput()
        pTab <- testRes()
        colType <- colClass()[input$seleFactor]

        if (is.null(pTab) || nrow(pTab) == 0) {
          return(NULL)
        }

        if (!is.null(input$testRes_row_last_clicked) &&
            input$testRes_row_last_clicked %in% seq_len(nrow(pTab))) {
          drugSelect <- pTab$drug[input$testRes_row_last_clicked]
        } else {
          drugSelect <- pTab$drug[1]
        }

        if (length(drugSelect) == 0 || is.na(drugSelect) || !drugSelect %in% plotTab$drug) {
          return(NULL)
        }

        plotTab <- filter(plotTab, drug == drugSelect)

        if (colType != "continuous") {
          p <- ggplot(plotTab, aes(x = status, y = viab, label = sampleID)) +
            geom_boxplot(width = 0.5, alpha = 0.5, aes(fill = status)) +
            geom_point() +
            ylab("Viability") +
            xlab(input$seleFactor) +
            ggtitle(as.character(drugSelect)) +
            theme_bw() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  text = element_text(size = 15))

          # hide outliers in the boxplot
          p <- plotly_build(p)
          p$x$data[[1]]$marker$opacity <- 0
          p$x$data[[2]]$marker$opacity <- 0

        } else {

          p <- ggplot(plotTab, aes(x = status, y = viab, label = sampleID)) +
            geom_point(col = "darkblue") +
            geom_smooth(method = "lm",
                        se = FALSE,
                        linetype = "dashed",
                        col = "firebrick") +
            ylab("Viability") +
            xlab(input$seleFactor) +
            ggtitle(as.character(drugSelect)) +
            theme_bw() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  text = element_text(size = 15))
        }

        ggplotly(p) %>%
          config(displayModeBar = FALSE)
      }
    })

    # text
    output$warningtext <- renderText(
      if (is.null(input$sampleAnnotations))
        paste("Please first select a feature for comparison in the Data Preprocessing tab,",
              "under the Sample Overview section, then choose sample information to display.")
    )


    ##############################################################################
    # Panel 6, summarize two drug combination
    ##############################################################################

    output$negDrugBox <- renderUI({

      choice <- screenData() %>%
        select(drug, drug2) %>%
        pull() %>%
        unique()

      selectInput("negDrug",
                  "Negative drug",
                  choice,
                  selected = choice[1])
    })

    output$synMethodBox <- renderUI({
      radioButtons("synMethod",
                   "Method for synergy effects",
                   choices = c("bliss", "bayesynergy"),
                   selected = "bliss")
    })


    # select combination index type
    output$CITypeBox <- renderUI({
      radioButtons("CIType",
                   "Type of Combination Index",
                   choices = c("CI", "logCI"),
                   selected = "CI")
    })


    # show option for incubation effect correction if 'normVal.cor' is present
    output$ifCorCheck4 <- renderUI({
      if ("normVal.cor" %in% colnames(screenData())) {
        checkboxInput("ifCorrected4",
                      "Incubation effect correction",
                      value = FALSE)
      }
    })

    # adapted from summariseScreenCombination, for bayesynergy method
    bayesMethod <- function(screenData, negDrug, ifCor = FALSE) {

      # check if the 'normVal' column is present for plate normalization
      if (!"normVal" %in% colnames(screenData)) {
        stop("Column 'normVal' not found.
         Plate normalization should be performed first")
      }

      # add check on negDrug
      # negative should be in the drug or drug2
      if(!negDrug %in% unique(c(screenData$drug, screenData$drug2))){
        stop("negDrug should be one of drug or drug2")
      }

      if(length(negDrug) > 1){
        stop("Only one negDrug is allowed")
      }

      if (!"bayesynergy" %in% installed.packages()[,"Package"]){
        stop("Please install bayesynergy package manually from Github:
           https://github.com/ocbe-uio/bayesynergy")
      }


      # only summarize the non-control wells
      subData <- dplyr::filter(screenData, wellType %in% "sample")

      allPair <- distinct(subData, sampleID, drug, drug2) %>%
        filter((!drug %in% negDrug) & (!drug2 %in% negDrug))

      allInput <- list()

      for(i in seq_len(nrow(allPair))){

        subIndex <- unlist(allPair[i, ])
        subTab <- dplyr::filter(subData, sampleID == subIndex[1]) %>%
          filter(drug %in% c(negDrug, subIndex[2]) &
                   drug2 %in% c(negDrug, subIndex[3])) %>%
          select(sampleID, drug, drug2, concentration, concentration2, normVal)

        valSel <- ifelse(ifCor, "normVal.cor", "normVal")
        # inputs for synergyscreen function
        allInput[[i]] <- list(y = as.matrix(subTab[ ,valSel]),
                              x = as.matrix(
                                subTab[ ,c("concentration", "concentration2")]),
                              drug_names = subIndex[2:3],
                              experiment_ID = subIndex[1])
      }

      # fit synergyscreen with parallel running
      fit <-
        bayesynergy::synergyscreen(allInput,
                                   save_raw = FALSE,
                                   save_plots = FALSE,
                                   bayesynergy_params = list(method = "sampling"))

      # get fit summary
      screenData <- fit$screenSummary %>%
        mutate(sampleID = `Experiment ID`,
               drug = `Drug A`,
               drug2 = `Drug B`,
               synergyScore = `Synergy Score`,
               antagonismScore = `Antagonism Score`) %>%
        select(sampleID, drug, drug2, synergyScore, antagonismScore) %>%
        left_join(x = screenData, y = .,
                  by = c("sampleID", "drug", "drug2"))

      return(
        list(screenData = screenData,
             fit = fit)
      )
    }


    bayesPlot <- reactiveValues()

    screenData_combine <- reactive({

      req(screenData())
      req(input$negDrug)
      req(input$synMethod)

      tryCatch({

        if (ifDrug2()){

          if (input$synMethod == "bayesynergy") {
            showModal(
              modalDialog("summarizing ...bayesynergy usually takes longer time",
                          footer = NULL))
          } else{
            showModal(modalDialog("summarizing ...", footer = NULL))
          }

          if (input$synMethod == "bliss") {
            screenData_combine <-
              summariseScreenCombination(screenData(),
                                         negDrug = input$negDrug,
                                         method = "bliss")
          }

          if (input$synMethod == "bayesynergy") {

            bayesObj <- bayesMethod(screenData(),
                                    negDrug = input$negDrug,
                                    ifCor = input$ifCorrected4)

            screenData_combine <- bayesObj$screenData
            bayesPlot$fit <- bayesObj$fit
          }

          removeModal()
        } else {
          screenData_combine <- screenData()
        }

        return(screenData_combine)
      }, error = function(e) {
        message("Error in summariseScreenCombination:")
        message(conditionMessage(e))  # to R console
        showNotification(conditionMessage(e), duration = NULL, type = "error")
        NULL
      })
    })

    sumCombTab <- reactive({

      req(screenData_combine())
      inCol <- c("drug", "drug2", "concentration", "concentration2")

      if (input$synMethod == "bliss") {

        if (input$CIType == "CI") {
          if (input$ifCorrected4) {
            inCol <- c(inCol, c("viab", "viab.cor", "viab2", "viab2.cor", "viabObs",
                                "viabExp", "CI", "CI.cor"))
          } else {
            inCol <- c(inCol, c("viab", "viab2", "viabObs", "viabExp", "CI"))
          }
        }

        if (input$CIType == "logCI") {

          if (input$ifCorrected4) {
            inCol <- c(inCol, c("viab", "viab.cor", "viab2", "viab2.cor", "viabObs",
                                "viabExp", "logCI", "logCI.cor"))

          } else {
            inCol <- c(inCol, c("viab", "viab2", "viabObs", "viabExp", "logCI"))
          }
        }
      }

      if ("bayesynergy" %in% input$synMethod) {
        inCol <- c(inCol, c("synergyScore", "synergyScore.cor", "antagonismScore",
                            "antagonismScore.cor"))
      }

      inCol <- intersect(inCol, colnames(screenData_combine()))
      inCol <- setdiff(inCol, "fileName")

      sumScreenCombine <- screenData_combine() %>%
        dplyr::select(c(input$sampleAnnotations, inCol)) %>%
        mutate_if(is.numeric, round, digits = 2) %>%
        mutate_if(is.character, as.factor)

      return(sumScreenCombine)
    })

    output$sumCombineTable <- DT::renderDT({
      req(sumCombTab())
      DT::datatable(sumCombTab(),
                    filter = "top",
                    rownames = FALSE,
                    selection = 'none',
                    style = "bootstrap")
    })

    ##############################################################################
    # Panel 7, two-drug combination plot
    ##############################################################################

    seleDrug <- function(input, output, session, screenData_combine, drugB, drugC) {

      ns <- session$ns
      output$seleBaseDrugBox <- renderUI({
        baseDrug <- ifelse(is.null(drugB()),
                           unique(screenData_combine$drug)[1], drugB())
        selectInput(ns("seleBaseDrug"), "Base Drug",
                    unique(screenData_combine$drug),
                    selected = baseDrug)
      })

      output$seleCombiDrugBox <- renderUI({
        combiDrug <- ifelse(is.null(drugC()),
                            unique(screenData_combine$drug2)[1], drugC())
        selectInput(ns("seleCombiDrug"), "Combined Drug",
                    unique(screenData_combine$drug2),
                    selected = combiDrug)
      })


      seleRes <- reactive(list(drugB = input$seleBaseDrug,
                               drugC = input$seleCombiDrug))

      return(seleRes)
    }

    selectedDrug <- reactiveValues(drugB = NULL, drugC = NULL)

    selectedDosePair <- callModule(seleDrug,
                                   "doseDrugPair",
                                   screenData_combine(),
                                   drugB = reactive(selectedDrug$drugB),
                                   drugC = reactive(selectedDrug$drugC))

    observeEvent(selectedDosePair(), {
      selectedDrug$drugB <- selectedDosePair()$drugB
      selectedDrug$drugC <- selectedDosePair()$drugC
    })


    # save g1, g2, g3 for pdf saving
    rv <- reactiveValues(g1 = NULL, g2 = NULL, g3 = NULL)
    # Dose Response Curve plot
    output$doseCurvPlot <- renderPlotly({

      drugB <- selectedDrug$drugB
      drugC <- selectedDrug$drugC

      # Creating the ggplot object
      rv$g1 <- ggplot(
        data = screenData_combine() %>%
          filter(drug == drugB, drug2 == drugC) %>%
          dplyr::distinct(sampleID, drug, concentration, drug2, concentration2,
                          .keep_all = TRUE) %>%
          select(concentration, concentration2, viab, viab2, viabObs, viabExp) %>%
          gather(key = "Group", value = "Viability", starts_with("viab")) %>%
          mutate(Group = ifelse(Group == "viab", paste(drugB, "(A)"),
                                ifelse(Group == "viab2", paste(drugC, "(B)"),
                                       ifelse(Group == "viabObs",
                                              paste(drugB, "+", drugC, "(AB)"),
                                              "Expected effect (A*B)")))) %>%
          mutate(Group = factor(Group,
                                levels = c(paste(drugB, "(A)"), paste(drugC, "(B)"),
                                           paste(drugB, "+", drugC, "(AB)"),
                                           "Expected effect (A*B)"))),
        aes(x = log(concentration),
            y = Viability,
            col = Group,
            group = Group,
            linetype = Group)) +
        stat_summary(fun.data = "mean_se",
                     fun.args = list(mult = 2),
                     geom = "errorbar",
                     width = 0.05,
                     position = position_dodge(width = 0.2)) +
        stat_summary(fun.y = "mean",
                     geom ="line",
                     fun.args = list(mult = 2),
                     position = position_dodge(width = 0.2)) +
        theme_bw(base_size = 20) +
        theme(legend.title = element_blank(),
              legend.position = "top") +
        ylim(c(0, 1.5)) +
        xlab(paste0("Log concentration of ", drugB)) +
        scale_color_manual(values = brewer.pal(name ="Set1", 9)[c(2, 3, 5, 4)]) +
        scale_linetype_manual(values = c(rep("solid", 3), "dashed"))

      # Convert to plotly for rendering
      ggplotly(rv$g1) %>%
        layout(legend = list(orientation = 'h',
                             x = 0.5, xanchor = 'center',
                             y = 1, yanchor = 'bottom',
                             ncol = 2,
                             title = "",
                             font = list(size = 18))) %>%
        config(displayModeBar = FALSE)

    })

    # Scatter Plot
    output$scatterPlot <- renderPlotly({

      drugB <- selectedDrug$drugB
      drugC <- selectedDrug$drugC

      plotDat <- screenData_combine() %>%
        filter(drug == drugB, drug2 == drugC) %>%
        dplyr::distinct(sampleID, drug, concentration, drug2, concentration2,
                        .keep_all = TRUE) %>%
        select(sampleID, concentration, concentration2, viabObs, viabExp)

      range <- c(0, max(c(plotDat$viabObs, plotDat$viabExp)))

      rv$g2 <- ggplot(plotDat, aes(x = viabObs, y = viabExp, color = sampleID)) +
        geom_point(alpha = 0.7) +
        geom_hline(aes(yintercept = 1), colour="grey", linetype = "dashed") +
        geom_vline(aes(xintercept = 1), colour="grey", linetype = "dashed") +
        facet_wrap( ~ concentration, ncol = 2) +
        geom_abline(intercept = 0, slope = 1, colour="black", linetype = "solid") +
        coord_fixed() +
        scale_x_continuous(limits = range) +
        scale_y_continuous(limits = range) +
        theme_bw(base_size = 12) +
        ylab(paste0("Measured viability")) +
        xlab(paste0("Expected viability")) +
        ggtitle(paste(drugB, "and", drugC)) +
        annotate("text", y = 0.8 * max(range), x = 0.3,
                 label= "synergy", size = 4, alpha = 0.4) +
        guides(col = FALSE)

      ggplotly(rv$g2) %>%
        config(displayModeBar = FALSE)

    })

    # Box Plot
    output$ciBoxplot <- renderPlotly({

      drugB <- selectedDrug$drugB
      drugC <- selectedDrug$drugC

      plotDat <- screenData_combine() %>%
        filter(drug == drugB, drug2 == drugC) %>%
        dplyr::distinct(sampleID, drug, concentration, drug2, concentration2,
                        .keep_all = TRUE) %>%
        select(sampleID, concentration, concentration2, CI, logCI) %>%
        mutate(concentration = factor(concentration))

      rv$g3 <- ggplot(plotDat, aes(x = concentration, y = CI)) +
        ggbeeswarm::geom_beeswarm(col = "#4DAF4A", alpha = 0.5) +
        geom_boxplot(outlier.shape = NA,
                     alpha = 0.2,
                     width = 0.3,
                     color = "black") +
        geom_hline(yintercept = c(-1, 0, 1), lty = "dashed") +
        theme_bw(base_size = 15) +
        xlab(paste0("Concentration of ", drugB)) +
        ylab(paste0("Combination index"))

      ggplotly(rv$g3) %>%
        config(displayModeBar = FALSE)

    })

    # Download PDF functionality
    output$download2 <- downloadHandler(

      filename = function() {
        paste0(input$plotType2, ".pdf")
      },

      content = function(file) {

        plot <- NULL
        # Get the ggplot objects for each plot
        if (input$plotType2 == 'Dose Response Curve') {
          plot <- rv$g1
        } else if (input$plotType2 == 'ScatterPlot: observe VS expected') {
          plot <- rv$g2
        } else if (input$plotType2 == 'Boxplot: CI by concentrations') {
          plot <- rv$g3
        }

        # Save the plot as a PDF
        ggsave(file, plot = plot, device = "pdf",
               width = input$figWidth2,
               height = input$figHeight2,
               limitsize = FALSE)
      }
    )

    # two-drug combination plot for bayesynergy

    output$seleGroup2 <- renderUI({

      req(screenData_combine())
      selectInput("selectSample",
                  "Sample Selection",
                  c("All samples", sort(unique(screenData_combine()$sampleID))),
                  selected = "All samples")
    })


    output$selePlotType3 <- renderUI({

      req(screenData_combine())
      req(input$selectSample)

      if (input$selectSample == "All samples") {
        selectInput("plotType3",
                    "Select Plots",
                    choices = c("Synergy Against Antagonism",
                                "Pairwise Drug Interaction",
                                "Synergy Per Experiment ID"),
                    selected = "Synergy Against Antagonism")
      } else {
        selectInput("plotType3",
                    "Select Plots",
                    choices = c("Synergy Against Antagonism",
                                "Pairwise Drug Interaction"),
                    selected = "Synergy Against Antagonism")
      }})

    #
    rv_bayes <- reactiveValues()
    output$bayesPlotPanel <- renderPlot({

      req(bayesPlot$fit)
      req(input$selectSample)
      req(input$plotType3)

      if (input$selectSample == "All samples") {
        whichExperimentID <- NULL
      } else {
        whichExperimentID <- input$selectSample
      }

      rv_bayes$plot <- plot_synergyscreen(
        x = bayesPlot$fit,
        whichExperimentID = whichExperimentID,
        whichPlot = input$plotType3
      )

      rv_bayes$plot
    })

    # Download PDF functionality
    output$download3 <- downloadHandler(

      filename = function() {
        paste0(input$plotType3, ".pdf")
      },

      content = function(file) {
        plot <- rv_bayes$plot
        # Save the plot as a PDF
        ggsave(file, plot = plot, device = "pdf",
               width = input$figWidth3,
               height = input$figHeight3,
               limitsize = FALSE)
      }
    )

    # Panel 8, Drug ranking
    output$rankScoreBox <- renderUI({
      req(filteredData())
      scoreCols <- intersect(c("DSS", "DSS1", "DSS2", "DSS3",
                               "AUC", "IC50", "meanViab"),
                             colnames(filteredData()))
      if (length(scoreCols) == 0) {
        return(tags$p("No summarization score columns available. Run normalization and summarization first."))
      }
      selectInput("rankScore", "Select score", choices = scoreCols, selected = scoreCols[1])
    })

    output$rankGroupBox <- renderUI({
      req(filteredData())
      groupChoices <- "All samples"
      if (!is.null(input$sampleAnnotations) && length(input$sampleAnnotations) > 0) {
        groupChoices <- c("All samples", input$sampleAnnotations)
      }
      selectInput("rankGroup", "Compare within", choices = groupChoices, selected = groupChoices[1])
    })

    output$rankSampleBox <- renderUI({
      req(filteredData())
      sampleIds <- sort(unique(filteredData()$sampleID))
      selectInput("rankSampleID", "Select sample", choices = sampleIds, selected = sampleIds[1])
    })

    rankData <- eventReactive(input$rankPlotButton, {
      req(filteredData(), input$rankScore, input$rankSampleID, input$rankSummary)
      data <- filteredData() %>% filter(wellType == "sample")
      scoreCol <- input$rankScore
      if (input$rankUseCorrected && paste0(scoreCol, ".cor") %in% colnames(data)) {
        scoreCol <- paste0(scoreCol, ".cor")
      }
      if (!scoreCol %in% colnames(data)) {
        stop("Selected score not available in the current data.")
      }

      data <- data %>%
        mutate(groupValue = if (input$rankGroup == "All samples") {
          "All samples"
        } else {
          as.character(.data[[input$rankGroup]])
        }) %>%
        select(sampleID, drug, groupValue, dplyr::all_of(scoreCol)) %>%
        distinct()

      groupSummary <- data %>%
        group_by(groupValue, drug) %>%
        summarise(groupScore = if (input$rankSummary == "mean") {
          mean(.data[[scoreCol]], na.rm = TRUE)
        } else {
          median(.data[[scoreCol]], na.rm = TRUE)
        }, .groups = "drop")

      sampleScores <- data %>%
        filter(sampleID == input$rankSampleID)
      if (nrow(sampleScores) == 0) {
        stop("Selected sample has no score data in the current filter.")
      }
      colnames(sampleScores)[which(colnames(sampleScores) == scoreCol)] <- "sampleScore"

      sampleScores %>%
        left_join(groupSummary, by = c("groupValue", "drug")) %>%
        mutate(diff = sampleScore - groupScore,
               rank = dplyr::dense_rank(desc(diff))) %>%
        arrange(desc(diff)) %>%
        head(as.integer(input$rankTopN))
    })

    output$rankWaterfall <- renderPlotly({
      req(rankData())
      df <- rankData() %>% mutate(drug = factor(drug, levels = unique(drug)))
      p <- ggplot(df, aes(x = drug, y = diff, fill = diff > 0)) +
        geom_col() +
        coord_flip() +
        scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "salmon"), guide = FALSE) +
        labs(x = "Drug",
             y = paste0(input$rankScore, " - ", input$rankSummary, " group"),
             title = paste0("Waterfall ranking for sample ", input$rankSampleID),
             subtitle = if (input$rankGroup == "All samples") "Compared to global summary" else paste0("Compared to ", input$rankGroup, " summary")) +
        theme_minimal()
      ggplotly(p) %>% config(displayModeBar = FALSE)
    })

    output$rankTable <- DT::renderDT({
      req(rankData())
      rankData() %>%
        select(rank, drug, sampleScore, groupScore, diff) %>%
        arrange(rank) %>%
        DT::datatable(rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
    })
  }
  return(serve)
}
