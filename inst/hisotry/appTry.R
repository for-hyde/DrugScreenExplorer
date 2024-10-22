# define modules
library(shinyr)
library(shiny)
library(tidyverse)
library(pheatmap)
library(RColorBrewer)
library(plotly)
library(dr4pl)
library(DrugScreenExplorer)
library(DT)

# load("./shinyData.RData")

ui <- navbarPage(

  "DrugScreenExplorer",
  inverse = TRUE,
  id = "tabs",

  tabPanel("Sample Overview",
           titlePanel("Overview of samples in the screen"),
           sidebarLayout(

             sidebarPanel(
               # import and zip
               fileInput("fileInput", "Choose ZIP File", accept = ".zip"),
               uiOutput("getRange"),
               textInput("sep",
                         "Choose a Range for Columns:",
                         value = "[;\t]"),
               checkboxInput("commaAsDecimal",
                             "Commas is used as decimal in the input data",
                             value = FALSE),
               checkboxInput("normalization",
                             "Calcualte percent inhibition",
                             value = TRUE),
               uiOutput("methodUI"),
               numericInput("discardLayer",
                            "The layers of discarded wells on the edge",
                            value = 0,
                            min = 0,
                            step = 1),

               checkboxInput("allowNA", "Allow missing values",
                             value = FALSE),
               uiOutput("selWells"),
               uiOutput("column_selector_ui"),
               width = 4),

             # main paneal
             mainPanel(
               tags$style(type = "text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"),
               DT::dataTableOutput("sampleInfo")
             )
           )),


  # Plate plot
  tabPanel("Plate plot",
           titlePanel("Heatmap plot of drug screen plates"),
           sidebarLayout(
             sidebarPanel(
               uiOutput("patientSeleBox"),
               uiOutput("sampleSeleBox"),
               uiOutput("plateSeleBox"),
               uiOutput("valueSeleRadio"),
               uiOutput("ifCensorCheck"),
               uiOutput("censorLimitBox"),
               uiOutput("ifCorCheck"), width = 4),
             mainPanel(tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"),
                       plotlyOutput("plateHeatmap", width = 800, height = 400),
                       column(dataTableOutput("infoTab"), width = 4),
                       column(plotOutput("linePlot", width = 400, height = 300),
                              width = 4), width = 8))
  ),

  # Panel 3
  tabPanel("Dose-response curves",
           titlePanel("Plot dose-response curves for selected drugs and samples"),
           sidebarLayout(
             sidebarPanel(
               uiOutput("nameDrugBox"),
               uiOutput("seleGroupBox"),
               conditionalPanel(condition = "input.selectGroup != 'All samples'"
                                ,uiOutput("seleFeatureBox")),
               uiOutput("colorBox"),
               uiOutput("ifCorCheck1"),
               checkboxInput("ifIC50", "Fit IC50 curve", value = FALSE)
             ),
             mainPanel(tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"),
                       plotlyOutput("curvePlot",width = 600, height= 400)
             ))),

  # panel 4
  tabPanel("Clustering",
           titlePanel("Clustering samples or drugs based on drug response"),
           sidebarLayout(
             sidebarPanel(
               uiOutput("seleGroup1"),
               conditionalPanel(condition = "input.selectGroup1 != 'All samples'"
                                ,uiOutput("seleFeature1")),
               uiOutput("sumTypeBox"),
               radioButtons("plotType","Plot type", c("Heatmap","PCA","t-SNE",
                                                      "Drug-drug correlation"),
                            selected = "Heatmap"),
               conditionalPanel(condition = "input.plotType == 't-SNE'",
                                numericInput("perplex", "Perplexity", value =20),
                                numericInput("Niter", "Iteration round", value = 1000)),
               conditionalPanel(condition = "input.plotType == 'Heatmap'",
                                radioButtons("ifRowNorm","Normalization",
                                             c("None","Row","Column"),
                                             selected = "None")),
               uiOutput("ifCorCheck2"),
               uiOutput("colorBox1"),
               checkboxInput("ifCensor1", "Fix value range", value = FALSE),
               uiOutput("censorLimitBox1"),
               sliderInput("topPercent", "Top variant drugs",
                           min = 10, max = 100, value = 100, step = 10),
               downloadButton("download", "Download as pdf"),
               numericInput("figWidth", label = "Figure width", value = 16),
               numericInput("figHeight", label = "Figure height", value = 15)
             ),
             mainPanel(
               tags$style(type="text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"
               ),
               actionButton("doPlot","Plot!", align = "center" ),
               plotOutput("clusterPlot", width=800, height = 800)
             ))),

  # Panel 5
  tabPanel("Association test",
           titlePanel("Test associations between drug responses and sample annotations"),
           sidebarLayout(
             sidebarPanel(uiOutput("seleGroup2"),
                          conditionalPanel(condition = "input.selectGroup2 != 'All samples'",
                                           uiOutput("seleFeature2")),
                          uiOutput("sumTypeBox1"),
                          uiOutput("factorBox"),
                          textOutput("methodBox"),
                          uiOutput("paramBox"),
                          uiOutput("ifCorCheck3")),
             mainPanel(dataTableOutput("testRes"),
                       plotlyOutput("diffPlot", width = 500,
                                    height = 500))
           ))
)


server <- function(input, output, session) {

  # get temp directory
  temp <- tempdir()

  observe({
    # unzip the file
    req(input$fileInput)

    unzip(input$fileInput$datapath, exdir = temp)
    fileCheck <- all(c("plateAnno", "rawData", "wellAnno") %in% list.files(temp))

    if (!fileCheck){
      misFiles <- setdiff(c("plateAnno", "rawData", "wellAnno"),
                          list.files(temp))
      warning(paste0("File missing: ", paste0(misFiles), "\n\n"))
    }
  })

  rawDir <- reactive({
    paste0(temp, "/rawData")
  })

  # check plateAnno
  checkPlateAnno <- reactive({

    req(input$fileInput)
    out <- list.files(paste0(temp, "/plateAnno/"),
                      pattern = "plateAnno",
                      full.names = TRUE)

    if (length(out) == 0)
      warning("Missing file in plateAnno folder: plateAnno, \n\n")

    return(out)
  })

  #check wellAnno
  checkWellAnno <- reactive({
    req(input$fileInput)
    out <- list.files(paste0(temp, "/wellAnno/"),
                      pattern = "wellAnno",
                      full.names = TRUE)
    if (length(out) == 0)
      warning("Missing file in wellAnno folder: wellAnno \n\n")

    return(out)
  })


  # row Range
  output$getRange <- renderUI({

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

  output$methodUI <- renderUI({
    if (input$normalization) {
      selectInput("method", "Methods to calculate percent inhibition",
                  choices = c('negatives', 'NPI'),
                  selected = "negatives")
    }
  })

  colRange <- reactive({
    if(input$colRight == 9999)
      input$colLeft
    else
      c(input$colLeft, input$colRight)
  })


  processedScreenData <- reactive({

    req(input$fileInput, input$rowTop, input$rowBottom, input$sep, input$discardLayer)
    processedScreenData <- readScreen(rawDir = rawDir(),
                                      plateAnnotationFile = checkPlateAnno(),
                                      wellAnnotationFile = checkWellAnno(),
                                      rowRange = c(input$rowTop, input$rowBottom),
                                      colRange = colRange(),
                                      sep = input$sep,
                                      commaAsDecimal = input$commaAsDecimal,
                                      negWell = c(),
                                      posWell = c(),
                                      allowNA = input$allowNA,
                                      discardLayer = input$discardLayer,
                                      normalization = FALSE)
  })


  ifDrug2 <- reactive({
    "drug2" %in% colnames(processedScreenData())
  })

  output$selWells <- renderUI({

    req(processedScreenData())
    # only two drug combination allowed
    if ("drug2" %in% colnames(processedScreenData())){
      allDrugs <- unique(paste0(processedScreenData()$drug, "_", processedScreenData()$drug2))
    } else {
      allDrugs <- unique(processedScreenData()$drug)
    }

    # selectInput
    tagList(
      selectInput("negWell",
                  "Negative control wells: drug names",
                  choices = sort(allDrugs),
                  multiple = T,
                  selected = NULL),
      selectInput("posWell",
                  "Positive wells: drug names",
                  choices = sort(allDrugs),
                  multiple = T,
                  selected = NULL)
    )
  })

  # define the well type information

  screenData <- reactive({

    req(processedScreenData())

    if (is.null(input$negWell))
      negWs <- c()
    else
      negWs <- input$negWell

    if (is.null(input$posWell))
      posWs <- c()
    else
      posWs <- input$posWell

    if (ifDrug2()){
      # Both drug_drug2 amd drug2_drug work
      combs <- paste0(processedScreenData()$drug, "_", processedScreenData()$drug2)
      screenData <- dplyr::mutate(processedScreenData(),
                                  wellType = case_when(combs %in% negWs ~ "neg",
                                                       combs %in% posWs ~ "pos",
                                                       .default = "sample"))
    } else {
      screenData <- dplyr::mutate(processedScreenData(),
                                  wellType = case_when(drug %in% negWs ~ "neg",
                                                       drug %in% posWs ~ "pos",
                                                       .default = "sample"))
    }

    # Normalize data
    if (input$normalization){

      if (input$method == "negatives" & is.null(input$negWell)) {
        return("Need negative drugs for normalization")
      }

      if (input$method == "NPI") {
        if (is.null(input$negWell) | is.null(input$posWell))
          return("Need negative and postive drugs for normalization")
      }

      screenData <- normalizePlate(
        screenData,
        method = input$method,
        discardLayer = input$discardLayer,
        bySample = FALSE
      )

      # summary data
      # ! need to set minConc = 0, maxConc = 15 setting
      screenData <- summariseScreen(screenData)
    }
    return(screenData)
  })

  # Dynamically generate UI for column selection
  output$column_selector_ui <- renderUI({

    req(screenData())
    choices <- setdiff(colnames(screenData()),
                       c("wellID", "rowID", "colID", "value"))

    if (!"sampleID" %in% colnames(screenData()))
      textOutput("'sampleID' should be defaulted selected")
    else
      selectInput("sampleAnnotations",
                  "Select sample information to display:",
                  choices = choices,
                  selected = c("sampleID", "fileName"),
                  multiple = TRUE)
  })


  # get infoTable

  infoTable <- reactive({

    req(screenData())
    req(input$sampleAnnotations)
    ## First panel: sample over view and subsetting
    ## A table to store unique samples information

    infoTable <- screenData() %>%
      dplyr::select(input$sampleAnnotations) %>%
      distinct(sampleID, .keep_all = TRUE)

    return(infoTable)
  })

  # # DT table for showing the sample annotations and filtering
  output$sampleInfo <- DT::renderDataTable({

    req(infoTable())
    infoTable_new <- mutate_if(infoTable(), is.character, as.factor)
    ## dplyr::select(-fileName)
    DT::datatable(infoTable_new,
                  filter = "top",
                  rownames = FALSE,
                  selection = 'none',
                  style = "bootstrap")
  })

  filteredData <- reactive({
    req(infoTable())
    selRows <- input$sampleInfo_rows_all
    selectedPlates <- as.character(infoTable()[selRows, ] %>%
                                     pull(fileName))
    dataNew <- filter(screenData(), fileName %in% selectedPlates)
    dataNew
  })

  ##############################################################################
  # Hide Table
  ##############################################################################

  ## hideTab
  # observe({
  #
  #   req(screenData())
  #   data <- screenData()
  #
  #   # Decide which panel to show or hide based on input data columns
  #   if (!all(c("sampleID", "normVal", "concentration", "drug") %in%
  #            colnames(data))) {
  #     hideTab(inputId = "tabs", target = "Dose-response curves")
  #   }
  #
  #   if (!all(c("normVal", "drug", "sampleID") %in% colnames(data))) {
  #     hideTab(inputId = "tabs", target = "Clustering")
  #   }
  #
  #   if (!all(c("normVal", "drug", "sampleID") %in% colnames(data))) {
  #     hideTab(inputId = "tabs", target = "Association test")
  #   }
  # })
  #
  #
  #
  # # Reactive object for getting filtered samples


  ##############################################################################
  ## Second panel: plate heatmap for a plate
  ##############################################################################

  # patients selection
  output$patientSeleBox <- renderUI({

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

    if ("normVal" %in% colnames(filteredData())) {
      opts <- c("viability", "z-score", "raw count")
    } else {
      opts <- c("z-score", "raw count")
    }
    radioButtons("ifNorm", "Select value type", opts)
  })

  output$ifCorCheck <- renderUI({

    if (!is.null(input$ifNorm) &&
        input$ifNorm == "viability" &&
        ("normVal.cor" %in% colnames(filteredData()))) {
      checkboxInput("ifCorrected",
                    "Incubation effect correction",
                    value = FALSE)
    }
  })

  output$ifCensorCheck <- renderUI({
    if (!is.null(input$ifNorm) && input$ifNorm %in% c("viability", "z-score")) {
      checkboxInput("ifCensor", "Fix value range", value = FALSE)
    }
  })

  output$censorLimitBox <- renderUI({

    if (!is.null(input$ifCensor) && input$ifCensor) {
      if (input$ifNorm == "viability") {
        upper = 1.5
        lower = 0
      } else if (input$ifNorm == "z-score") {
        upper = 4
        lower = -4
      }

      tagList(div(style = "display:inline-block; width: 90px",
                  textInput("upperLimit", "upper limit", value = upper)),
              div(style = "display:inline-block; width: 90px",
                  textInput("lowerLimit", "lower limit", value = lower)))
    }
  })

  plotTab <- reactive({

    req(input$selePlate)
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

    nCol <- length(unique(screenData()$colID))
    nRow <- length(unique(screenData()$rowID))
    rowSeq <- genRowIDs(nRow)
    colSeq <- genColIDs(nCol)
    matPlate <- filter(screenData(), fileName == input$selePlate) %>%
      mutate(rowID = factor(rowID, levels = rev(rowSeq))) %>%
      mutate(colID = factor(colID, levels = colSeq))

    if (input$ifNorm == "viability") {
      if (!is.null(input$ifCorrected) && input$ifCorrected)
        matPlate <- mutate(matPlate, val = normVal.cor)
      else
        matPlate <- mutate(matPlate, val = normVal)
    } else if (input$ifNorm == "raw count") {
      matPlate <- mutate(matPlate, val = value)
    } else if (input$ifNorm == "z-score") {
      matPlate <- mutate(matPlate, val = (value - mean(value)) / sd(value))
    }

    if (!is.null(input$ifCensor) && input$ifCensor) {
      matPlate <- mutate(matPlate,
                         val = if_else(val > as.numeric(input$upperLimit),
                                       as.numeric(input$upperLimit), val)) %>%
        mutate(val = if_else(val < as.numeric(input$lowerLimit),
                             as.numeric(input$lowerLimit), val))
    }
    matPlate
  })

  # plot the heatmap
  output$plateHeatmap <- renderPlotly({

    req(input$selePlate)
    p <- ggplot(plotTab(),
                aes(x = colID, y = rowID, fill = val)) +
      geom_tile(color = "gray", size = 3) +
      theme_minimal() +
      theme(axis.text = element_text(size = 6),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.margin = margin(5, 5, 5, 5)) +
      labs(x = "", y = "", title = input$selePlate)

    if (input$ifNorm %in% c("viability", "z-score")) {

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
  output$infoTab <- renderDataTable({

    s <- event_data("plotly_click")
    # Add potential interesting columns that may or may not exist

      addCol <- intersect(c("drug", "drug2", "wellType",
                            "concentration", "concentration2"),
                          colnames(plotTab()))

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
  output$linePlot <- renderPlot({

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
        theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "red"),
              axis.text  = element_text(size = 13),
              axis.title = element_text(size = 13))

      if (input$ifCensor) {
        p <- p + coord_cartesian(ylim = c(as.numeric(input$lowerLimit),
                                          as.numeric(input$upperLimit)))
      }
    }
    p
  })


  ##############################################################################
  ## Third panel: dose-response curves
  ##############################################################################

  ## select patient group for subsetting
  output$seleGroupBox <- renderUI({
    # req(input$sampleAnnotations)
    selectInput("selectGroup",
                "Subset samples by",
                c("All samples", input$sampleAnnotations),
                selected = "All samples")
  })

  # select a drug name
  output$nameDrugBox <- renderUI({
    # req(filteredData())
    allDrugs <- filter(filteredData(),
                       wellType == "sample") %>%
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

  # ?
  colClass <- reactive({

    req(input$sampleAnnotations)
    colRes <- sapply(input$sampleAnnotations,
                     function(n) {
                       DrugScreenExplorer:::detectClass(
                         filteredData() %>%
                           as.data.frame() %>%
                           dplyr::distinct(sampleID, .keep_all = TRUE) %>%
                           dplyr::select(all_of(n)) %>%
                           pull())
                     })

    print(colRes)
    return(colRes)
  })

  ## select features for color scheme
  output$colorBox <- renderUI({
    req(input$sampleAnnotations)
    sampleAnnotations <- input$sampleAnnotations[colClass() != "nd"]
    selectInput("seleColor", "Color by", sampleAnnotations,
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
    # req(input$nameDrug)
    # subset the screen data according to the selection
    # if (!is.null(input$selectGroup) && input$selectGroup == "All samples") {
    if (input$selectGroup == "All samples") {
      plotTab <- filter(filteredData(),
                        drug == input$nameDrug)
    } else {

      selePlates <- filteredData() %>%
        filter(!!sym(input$selectGroup) %in% input$seleSub) %>%
        select(fileName)

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
      geom_point(pch = 16, size = 4) +
      theme_classic() +
      scale_x_log10(labels = fmt_dcimals(3)) +
      theme(axis.line.x = element_line(linewidth = 0.5),
            plot.title = element_text(
              hjust = 0.5,
              face = "bold",
              color = "red")) +
      ylab("Viability") +
      xlab("Concentration") +
      coord_cartesian(ylim = c(0, 1.5)) +
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

    p
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
                c("All samples", input$sampleAnnotations))
  })

  # select the summarisation statistics
  output$sumTypeBox <- renderUI({
    typeMap <- structure(c("meanViab", "AUC", "IC50"),
                         names = c("average viability", "AUC", "IC50"))
    typeAvail <- typeMap[typeMap %in% colnames(filteredData())]
    radioButtons("seleSumType", "Drug effect summarisation method", typeAvail)
  })

  # select features for grouping of the patients
  output$seleFeature1 <- renderUI({
    if (!is.null(input$selectGroup1) && input$selectGroup1 != "All samples") {
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
    sampleAnnotations <- input$sampleAnnotations[colClass() != "nd"]
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
      upper = 1.5
      lower = 0
      tagList(div(style = "display:inline-block; width: 90px",
                  textInput("upperLimit1", "upper limit", value = upper)),
              div(style = "display:inline-block; width: 90px",
                  textInput("lowerLimit1", "lower limit", value = lower)))
    }
  })

  # table for the clustering plot on panel 3
  plotMat <- reactive({
    # subset the screen data according to the selection
    if (input$selectGroup1 == "All samples") {
      plotTab <- filter(filteredData(), wellType == "sample")
    } else {
      selePlates <- filteredData() %>%
        filter(!!sym(input$selectGroup1) %in% input$seleSub1) %>%
        select(fileName)
      # [unlist(filteredData()[, input$selectGroup1]) %in% , ]$fileName
      plotTab <- filter(filteredData(), fileName %in% selePlates, wellType == "sample")
    }

    if (!is.null(input$ifCorrected2)) {
      if (input$ifCorrected2)
        plotTab <- mutate(plotTab, viab = plotTab[[paste0(input$seleSumType, ".cor")]])
    } else {
      plotTab <- mutate(plotTab, viab = plotTab[[input$seleSumType]])
    }

    if (input$ifCensor1) {
      plotTab <- plotTab %>%
        mutate(viab = ifelse(viab > as.numeric(input$upperLimit1),
                             as.numeric(input$upperLimit1), viab)) %>%
        mutate(viab = ifelse(viab < as.numeric(input$lowerLimit1),
                             as.numeric(input$lowerLimit1), viab))
    }

    viabMat <- group_by(plotTab, drug, sampleID) %>%
      summarise(viab = mean(viab))

    # print("928")
    # print(viabMat)

    # idMap <- list(origin = as.character(viabMat$sampleID))
    idMap <- tibble(origin = as.character(unique(viabMat$sampleID)))
    # print("idMap")
    # print(idMap)

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
    tsneRes <- Rtsne(distViab,
                     perplexity = input$perplex,
                     theta = 0,
                     max_iter = input$Niter,
                     is_distance = TRUE,
                     dims = 2)
    tsneRes <- tsneRes$Y
    rownames(tsneRes) <- labels(distMat)
    colnames(tsneRes) <- c("x", "y")
    plotTab <- data.frame(tsneRes)
    rownames(plotTab) <- colnames(viabMat)
    plotTab
  })

  # prepare the clustering plot on panel 3
  plotCluster <- eventReactive(input$doPlot, {

    viabData <- filteredData()
    viabMat <- plotMat()$viabMat
    idMap <- plotMat()$idMap

    # why play with the sampleID
    annoCol <- tibble(sampleID_new = colnames(viabMat),
                      sampleID = idMap[match(colnames(viabMat), idMap$new), ]$origin) %>%
      left_join(select(viabData, sampleID, input$seleColor1), by = "sampleID") %>%
      select(-sampleID) %>%
      distinct(sampleID_new, .keep_all = TRUE) %>%
      as.data.frame() %>%
      column_to_rownames("sampleID_new")

    # pritn(annoCol)

    if (input$plotType == "Heatmap") {

      # perform hierachical clustering on columns
      hc <- hclust(as.dist(1 - cor(viabMat)), method = "ward.D2")
      hr <- hclust(as.dist(1 - cor(t(viabMat))), method = "ward.D2")
      # hc <- hclust(dist(t(viabMat), method = 'euclidean'), method = 'ward.D2')

      # perfrom hierachical clustering on rows, normalized or not
      if (input$ifRowNorm == "Row") {
        viabMat <- t(scale(t(viabMat)))
      } else if (input$ifRowNorm == "Column") {
        viabMat <- scale(viabMat)
      }

      annoCol <- annoCol[, colSums(!is.na(annoCol)) != 0, drop = FALSE]

      if (ncol(annoCol) != 0) {
        g <- pheatmap(viabMat,
                      color = colorRampPalette(brewer.pal(n = 7, name = "RdYlBu"))(100),
                      scale = "none",
                      annotation_col = annoCol,
                      cluster_rows = hr,
                      cluster_cols = hc,
                      silent = TRUE)
      } else {
        # if nothing to annotate
        g <- pheatmap(viabMat,
                      color = colorRampPalette(brewer.pal(n = 7, name = "RdYlBu"))(100),
                      scale = "none",
                      cluster_rows = hr,
                      cluster_cols = hc,
                      silent = TRUE)
      }
      g

    } else if (input$plotType == "Drug-drug correlation") {
      g <- pheatmap(cor(t(viabMat), method = "spearman"), silent = TRUE)
      g
    } else {
      colType <- colClass()[input$seleColor1]
      if (input$plotType == "PCA") {
        pcaTab <- cbind(tabPCA()$pcaTab, annoCol)
        varExp <- tabPCA()$varExp
        pcaTab$sampleID <- rownames(pcaTab)

        if (colType != "continuous")
          pcaTab[[input$seleColor1]] <- as.factor(pcaTab[[input$seleColor1]])

        g <- ggplot(pcaTab,
                    aes_string(x = "PC1", y = "PC2",
                               color = input$seleColor1)) +
          geom_point() +
          theme_bw() +
          xlab(sprintf("PC1 (%2.1f%%)", varExp[1])) +
          ylab(sprintf("PC2 (%2.1f%%)", varExp[2]))

      } else if (input$plotType == "t-SNE") {
        plotTab <- cbind(tabTSNE(), annoCol)
        plotTab$sampleID <- rownames(plotTab)
        if (colType != "continuous")
          plotTab[[input$seleColor1]] <- as.factor(plotTab[[input$seleColor1]])

        g <- ggplot(plotTab, aes_string(x = "x", y = "y", color = input$seleColor1)) + geom_point() + theme_bw()

      }

      if (colType == "continuous") {
        g <- g + scale_color_gradient(low = "blue", high = "red")
      }
     g
    }
  })

  # clustering plot on panel 3
  output$clusterPlot <- renderPlot({
    plotCluster()
  })

  # download the cluster plot
  output$download <- downloadHandler(
    filename = paste0(input$plotType, ".pdf", sep = ""),
    content = function(file) {
      ggsave(file, plot = plotCluster(), device = "pdf",
             width = input$figWidth, height = input$figHeight, limitsize = FALSE)
    })

  ##############################################################################
  # Panel 5
  ##############################################################################

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
    typeMap <- structure(c("meanViab", "AUC", "IC50"),
                         names = c("average viability", "AUC", "IC50"))
    typeAvail <- typeMap[typeMap %in% colnames(filteredData())]
    radioButtons("seleSumType1",
                 "Drug effect summarisation method",
                 typeAvail)
  })

  # select features for hypothesis testing
  output$factorBox <- renderUI({

    sampleAnnotations <- input$sampleAnnotations[colClass() != "nd"]
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
      paste0("Test used: ", typeMap[colType])
    }
  })

  # text box to show the model for testing
  output$paramBox <- renderUI({

    if (!is.null(input$seleFactor)) {

      print(input$seleFactor)
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


    if (!is.null(input$seleFactor)) {
      # subset the screen data according to the selection
      if (!is.null(input$selectGroup2) && input$selectGroup2 == "All samples") {
        plotTab <- filter(filteredData(), wellType == "sample")
      } else {
        selePlates <- filteredData()[unlist(filteredData()[, input$selectGroup2]) %in% input$seleSub2, ]$fileName
        plotTab <- filter(filteredData(), fileName %in% selePlates, wellType == "sample")
      }

      if (!is.null(input$ifCorrected3)) {
        if(input$ifCorrected3)
          plotTab <- mutate(plotTab, viab = plotTab[[paste0(input$seleSumType1, ".cor")]])
        else
          plotTab <- mutate(plotTab, viab = plotTab[[input$seleSumType1]])
      } else {
        plotTab <- mutate(plotTab, viab = plotTab[[input$seleSumType1]])
      }

      colType <- colClass()[input$seleFactor]
      plotTab <- group_by(plotTab, drug, sampleID) %>%
        summarise(viab = mean(viab)) %>%
        ungroup() %>%
        mutate(status = filteredData()[match(sampleID, filteredData()$sampleID), ][[input$seleFactor]]) %>%
        filter(!is.na(status))

      if (colType != "continuous")
        plotTab$status <- as.factor(as.character(plotTab$status))

      print(plotTab)

      plotTab
    }
  })
  # reactive object to perform the test
  testRes <- reactive({

    if (!is.null(input$seleFactor) && !is.null(testInput())) {

      colType <- colClass()[input$seleFactor]
      testTab <- testInput() %>%
        group_by(drug) %>%
        nest()

      if (colType == "binary") {

        testParam <- input$seleParam == "Equal variance"
        res <- mutate(testTab, model = map(data, ~t.test(viab ~ status, ., var.equal = TRUE))) %>%
          mutate(out = map(model, broom::tidy)) %>%
          unnest(out) %>%
          ungroup() %>%
          mutate(diffMean = estimate2 - estimate1, adjusted.p = p.adjust(p.value, method = "BH")) %>%
          select(drug, diffMean, p.value, adjusted.p) %>%
          arrange(p.value)

      } else if (colType == "continuous") {

        testParam <- ifelse(input$seleParam != "Nonparametric (Spearman)", "pearson", "spearman")
        res <- mutate(testTab,
                      model = map(data,
                                  ~cor.test(~viab + status, ., method = testParam))) %>%
          mutate(out = map(model, broom::tidy)) %>%
          unnest(out) %>%
          ungroup() %>%
          mutate(coefficient = estimate, adjusted.p = p.adjust(p.value, method = "BH")) %>%
          select(drug, coefficient, p.value, adjusted.p) %>%
          arrange(p.value)

      } else if (colType == "categorical") {

        res <- mutate(testTab, model = map(data, ~ aov(lm(viab ~ status, .)))) %>%
          mutate(out = map(model, broom::tidy)) %>%
          unnest(out) %>%
          ungroup() %>%
          filter(term == "status") %>%
          mutate(adjusted.p = p.adjust(p.value, method = "BH")) %>%
          select(drug, statistic, p.value, adjusted.p) %>%
          arrange(p.value)
      }

      print(res)
      res
    }
  })

  # render the output table
  output$testRes <- renderDataTable({
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
      print(colType)

      if (!is.null(input$testRes_row_last_clicked)) {
        drugSelect <- pTab$drug[input$testRes_row_last_clicked]

      } else {
        drugSelect <- pTab$drug[1]
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
          geom_smooth(method = "lm", se = FALSE, linetype = "dashed", col = "firebrick") +
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
}

shinyApp(ui = ui, server = server)




