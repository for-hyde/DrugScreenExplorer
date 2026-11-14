
seleDrugUI <- function(id) {
  ns <- NS(id)
  tagList(tags$h4("Select drugs", style = "color: #2166AC"),
          uiOutput(ns("seleBaseDrugBox")),
          uiOutput(ns("seleCombiDrugBox")))
}

# Set global error handler
options(shiny.error = function() {
  # This prevents the app from shutting down
  # Errors will be handled by individual tryCatch blocks
})

# library(shinyr)
library(shiny)
library(tidyverse)
library(pheatmap)
library(RColorBrewer)
library(plotly)
library(dr4pl)
library(DrugScreenExplorer)
library(DT)
library(shinyBS)
library(Rtsne)

ui <- navbarPage(

  "DrugScreenExplorer",
  inverse = TRUE,
  id = "tabs",

  tabPanel("Data Preprocessing",
           titlePanel("Data Preprocessing"),
           sidebarLayout(

             sidebarPanel(
               # import and zip

               tags$div(

                 style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",
                 h3(tags$b("1 File Import"), style = "background-color: lightgray; padding: 10px;"),

                 fileInput("fileInput", "Choose ZIP File", accept = ".zip"),
                 bsTooltip(id = "fileInput", title = "A Zip File containing plateAnno, wellAnno, and rawData"),
                 textOutput("pathDisplay") %>%
                   tagAppendAttributes(style = "color: gray; margin-top: 0; font-size: 10px"),

                 uiOutput("getRange"),
                 bsTooltip("getRange", "The range of lines and columns containing actual values"),
                 uiOutput("sepText"),
                 bsTooltip("sepText", "Delimiters for extracting values in the text file"),
                 uiOutput("commaAsDecimalCheckBox"),
                 bsTooltip("commaAsDecimalCheckBox", "Whether commas is used as decimal"),
                 actionButton("doImport", "Import", align = "center"),

                 uiOutput("column_selector_ui")
               ),


               tags$div(

                 style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",
                 h3(tags$b("2 Normalization"), style = "background-color: lightgray; padding: 10px;"),

                 # uiOutput("normCheckBox"),
                 # bsTooltip("normCheckBox", "Whether to calculate percent inhibition"),
                 uiOutput("methodUI"),
                 bsTooltip("methodUI", "If negatives, each measurement on the plate will be divided by the median of the values of negative control wells on the same plate. If NPI, a normalized percent inhibition (NPI) is applied in a per-plate basis"),
                 uiOutput("discardLayerInput"),
                 bsTooltip("discardLayerInput", "To avoid the impact of incubation effect on the negative controls, a certain number of the exterior wells on the plate can be ignored when summarising the measurement of controls"),
                 uiOutput("allowNACheckBox"),
                 uiOutput("selWells"),
                 bsTooltip("selWells", "Specifies negative /positive control wells"),
                 actionButton("doNorm", "Normalize", align = "center"),
                 bsTooltip("doNorm", "Please wait, the process may take some time"),
                 verbatimTextOutput("warningMessage") %>%
                   tagAppendAttributes(style = "color: red; margin-top: 10px;"),
                 uiOutput("column_selector_normTable_ui")
               ),

               tags$div(

                 style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",
                 h3(tags$b("3 Edge Effects Adjustment"), style = "background-color: lightgray; padding: 10px;"),

                 uiOutput("estimateMethod"),
                 bsTooltip("estimateMethod", "'loess', uses local regression to estimate edge effect; 'sigmoid', use 2D-sigmoid model to estimate edge effect"),
                 uiOutput("useNeg"),
                 bsTooltip("useNeg", "If TRUE, only negative controls are used to estimate edge effect. Otherwise, all the wells are used for edge effect estimation"),
                 uiOutput("span"),
                 bsTooltip("span", "Controls the degree of smoothing of loess"),
                 uiOutput("correctMethod"),
                 textOutput("correctEdgeEffect_message") %>%
                   tagAppendAttributes(style = "color: red; margin-top: 10px;"),
                 bsTooltip("correctMethod", "Currently two methods 'bliss' and 'linear' are supported. If bliss is selected, the corrected values are simply the normalized signal intensity values divided by the estimated incubation/edge effect intensity. If linear is selected, the correction process will be smoothed by a linear function depends on the original normalized signal intensity. This option is suitable for the situation when the edge effect is very strong and results in instability in the correction process."),
                 actionButton("doCorrEdge", "Correct", align = "center")
               ), width = 4),

             # main paneal
             mainPanel(
               tags$style(HTML(".table-container {padding: 10px; border-radius: 5px; margin-bottom: 20px; }
               .table1 {background-color: #f8f9fa;  /* Light gray background */ }
               .table2 {background-color: #f8f9fa;  /* Slightly darker gray background */}
               h3 {margin-top: 0;  /* Keep title close to the table */
                   margin-bottom: 10px;  /* Adjust space between title and table */
                   text-align: center;  /* Center the title */
                   font-weight: bold;  /* Make the title bold */ /* Remove the top margin from h3 to keep it close to the table */
                   background-color: lightgray;  /* Light gray background */
                   padding: 10px;  /* Add some padding to the title */
                   border-radius: 5px;  /* Rounded corners */} ")),

               textOutput("readScreen_message"),
               uiOutput("sampleInfo_ui"),
               uiOutput("normTable_ui")
             )
           )
  ),

  # Plate plot
  tabPanel("Plate plot",
           titlePanel("Heatmap plot of drug screen plates"),
           sidebarLayout(
             sidebarPanel(

               tags$div(
                 style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",
                 h3(tags$b("Sample Selection"), style = "background-color: lightgray; padding: 10px;"),

                 uiOutput("patientSeleBox"),
                 uiOutput("sampleSeleBox"),
                 uiOutput("plateSeleBox")
               ),

               tags$div(
                 style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",
                 h3(tags$b("Values to exhibit"), style = "background-color: lightgray; padding: 10px;"),

                 uiOutput("valueSeleRadio"),
                 uiOutput("ifCensorCheck"),
                 bsTooltip("ifCensorCheck", "Set lower and upper limits"),
                 uiOutput("censorLimitBox"),
                 bsTooltip("censorLimitBox", "Lower or Upper limits for selected values"), width = 4)
               # uiOutput("ifCorCheck"))
             ),

             mainPanel(tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"),
                       plotlyOutput("plateHeatmap", width = 800, height = 400),
                       column(dataTableOutput("infoTab"), width = 4),
                       column(plotlyOutput("detailPlot", width = 400, height = 300),
                              width = 4), width = 8))
  ),

  # Panel 3
  tabPanel("Dose-response curves",
           titlePanel("Plot dose-response curves for selected drugs and samples"),
           sidebarLayout(
             sidebarPanel(
               uiOutput("nameDrugBox"),
               uiOutput("seleGroupBox"),
               conditionalPanel(condition = "input.selectGroup != 'All samples'",
                                uiOutput("seleFeatureBox")),
               uiOutput("colorBox"),
               uiOutput("ifCorCheck1"),
               checkboxInput("ifIC50", "Fit IC50 curve", value = FALSE)
             ),
             mainPanel(tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"),
                       plotlyOutput("curvePlot", width = 600, height= 400)
             ))),

  # panel 4
  tabPanel("Clustering",
           titlePanel("Clustering samples or drugs based on drug response"),
           sidebarLayout(
             sidebarPanel(

               tags$div(
                 style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",

                 uiOutput("seleGroup1"),
                 conditionalPanel(condition = "input.selectGroup1 != 'All samples'", uiOutput("seleFeature1")),
                 uiOutput("sumTypeBox"),
                 radioButtons("plotType",
                              "Plot type",
                              c("Heatmap","PCA","t-SNE", "Drug-drug correlation"),
                              selected = "Heatmap"),
                 conditionalPanel(condition = "input.plotType == 't-SNE'",
                                  numericInput("perplex", "Perplexity", value =20),
                                  numericInput("Niter", "Iteration round", value = 1000)),
                 conditionalPanel(condition = "input.plotType == 'Heatmap'",
                                  radioButtons("ifRowNorm",
                                               "Normalization",
                                               c("None","Row","Column"),
                                               selected = "None")),
                 uiOutput("ifCorCheck2"),
                 uiOutput("colorBox1"),
                 checkboxInput("ifCensor1", "Fix value range", value = FALSE),
                 uiOutput("censorLimitBox1"),
                 sliderInput("topPercent", "Top variant drugs",
                             min = 10, max = 100, value = 100, step = 10),
                 actionButton("doPlot", "Plot!", align = "center" )
               ),

               tags$div(
                 style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",
                 numericInput("figWidth", label = "Figure width", value = 16),
                 numericInput("figHeight", label = "Figure height", value = 15),
                 downloadButton("download", "Download as pdf")
               )
             ),
             mainPanel(
               textOutput("tsne_message"),
               plotOutput("clusterPlot", width = 800, height = 800)
             ))),

  # Panel 5
  tabPanel("Association test",
           titlePanel("Test associations between drug responses and sample annotations"),
           sidebarLayout(
             sidebarPanel(
               tags$div(
                 style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",
                 uiOutput("seleGroup2"),
                 conditionalPanel(condition = "input.selectGroup2 != 'All samples'",
                                  uiOutput("seleFeature2")),
                 uiOutput("sumTypeBox1"),
                 uiOutput("factorBox")
               ),

               tags$div(
                 style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",
                 div(
                   style = "font-weight: bold; font-size: 16;",
                   textOutput("methodBox")  # Regular textOutput inside a div
                 ),
                 tags$br(),
                 uiOutput("paramBox"),
                 uiOutput("ifCorCheck3"))
             ),

             mainPanel(dataTableOutput("testRes"),
                       textOutput("warningtext"),
                       uiOutput("errorText"),
                       plotlyOutput("diffPlot", width = 500, height = 500))
           )),

  # Panel 6
  tabPanel("Overview of Two-Drug Combination",
           titlePanel("Overview of drug combination effects"),
           sidebarLayout(
             sidebarPanel(uiOutput("negDrugBox"),
                          bsTooltip("negDrugBox", "The viability indeed reflects the effects of a single drug, if the drug is combined with the negative drug."),
                          uiOutput("synMethodBox"),
                          bsTooltip("synMethodBox", "Specify the method to summarize the synergy or antagonism effects: bliss and bayesynergy."),

                          conditionalPanel(
                            condition = "input.synMethod == 'bliss'",
                            uiOutput("CITypeBox")),
                          bsTooltip("CITypeBox", "CI = observed viability - expected viability; \n logCI = log10(observed viability / expected viability)"),
                          uiOutput("ifCorCheck4"),
                          bsTooltip("ifCorCheck4", "Should the combination index be calculated using incubation effect adjusted viability?")
             ),

             mainPanel(
               tags$style(type = "text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"),
               DT::dataTableOutput("sumCombineTable"),

               conditionalPanel(
                 condition = "input.synMethod == 'bliss' & input.CIType == 'CI'",
                 tags$div(style = "margin-top:10px; font-style: italic; color: gray;",
                          "Note: CI = viabObs - viabExp; viabOb, observed viability; viabExp, expected viability")),

               conditionalPanel(
                 condition = "input.synMethod == 'bliss' & input.CIType == 'logCI'",
                 tags$div(style = "margin-top:10px; font-style: italic; color: gray;",
                          "Note: logCI = log10(viabObs / viabExp); viabObs, observed viability; viabExp, expected viability")))
           )),

  tabPanel("Combination hypothesis test",
           titlePanel("Compare combination index values between metadata groups"),
           sidebarLayout(
             sidebarPanel(uiOutput("combHypothesisControls")),
             mainPanel(
               uiOutput("combHypothesisError"),
               DT::dataTableOutput("combHypothesisResults"),
               plotlyOutput("combHypothesisPlot", width = 800, height = 600)
             )
           )),

  # Panel 7
  tabPanel("Drug Combination Effects",
           titlePanel("Combination Effects"),

           sidebarLayout(
             sidebarPanel(

               conditionalPanel(
                 condition = "input.synMethod == 'bliss'",

                 tags$div(
                   style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",
                   seleDrugUI("doseDrugPair"),
                   bsTooltip("doseDrugPair", "Two-drug combination"),
                   selectInput("plotType2",
                               "Select plot",
                               c("Dose Response Curve", "ScatterPlot: observe VS expected", "Boxplot: CI by concentrations")),
                   bsTooltip("plotType2", "Results to exhibit")
                 ),

                 tags$div(
                   style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",
                   numericInput("figWidth2", label = "Figure width", value = 12),
                   numericInput("figHeight2", label = "Figure height", value = 8),
                   downloadButton("download2", "Download as pdf")
                 )
               ),

               conditionalPanel(

                 condition = "input.synMethod == 'bayesynergy'",

                 tags$div(

                   style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",
                   uiOutput("seleGroup2"),
                   bsTooltip("seleGroup2", "Choose the sample group(s) to display results for"),
                   uiOutput("selePlotType3"),
                   bsTooltip("selePlotType3", "Select the type of plot you want to generate")
                 ),

                 tags$div(
                   style = "border: 1px solid #ddd; padding: 10px; margin-bottom: 10px;",
                   numericInput("figWidth3", label = "Figure width", value = 12),
                   numericInput("figHeight3", label = "Figure height", value = 8),
                   downloadButton("download3", "Download as pdf")
                 )
               )),


             mainPanel(
               conditionalPanel(condition = "input.plotType2 == 'Dose Response Curve' && input.synMethod == 'bliss'",
                                plotlyOutput("doseCurvPlot", width = 1000, height = 800)),

               conditionalPanel(condition = "input.plotType2 == 'ScatterPlot: observe VS expected' && input.synMethod == 'bliss'",
                                plotlyOutput("scatterPlot", width = 1000, height = 800)),

               conditionalPanel(condition = "input.plotType2 == 'Boxplot: CI by concentrations' && input.synMethod == 'bliss'",
                                plotlyOutput("ciBoxplot", width = 1000, height = 800)),

               conditionalPanel(condition = "input.synMethod == 'bayesynergy'",
                                plotOutput("bayesPlotPanel", width = 1000, height = 800)),

             )
           ))
)


server <- function(input, output, session) {

  output$pathDisplay <- renderText({
    paste0("Find example data at ", system.file(package = "DrugScreenExplorer"), "/extdata")
  })

  # get temp directory
  temp <- tempdir()

  # Arguments for selection
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
    textInput("sep",
              "Separator Character",
              value = "[;\t]")
  })

  output$commaAsDecimalCheckBox <- renderUI({
    req(input$fileInput)
    checkboxInput("commaAsDecimal",
                  "Commas is used as decimal in the input data",
                  value = FALSE)
  })


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

  colRange <- reactive({
    if(input$colRight == 9999)
      input$colLeft
    else
      c(input$colLeft, input$colRight)
  })


  #-----------------------------------------------------------------------------
  # 1.1 import data

  processedScreenData <- eventReactive(input$doImport, {

    req(input$fileInput,
        input$rowTop,
        input$rowBottom,
        input$sep,
        input$discardLayer)

    processedScreenData <-
      tryCatch({result <- readScreen(rawDir = rawDir(),
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
      }, error = function(e) {
        message <- paste0("Error: ", conditionMessage(e))
        list(result = NULL, message = message)
      })

    return(processedScreenData)
  })

  output$readScreen_message <- renderText({
    req(processedScreenData())
    if (!is.null(processedScreenData()$message))
      processedScreenData()$message
  })

  # Dynamically generate UI for column selection
  output$column_selector_ui <- renderUI({

    req(processedScreenData())
    choices <- setdiff(colnames(processedScreenData()),
                       c("wellID", "rowID", "colID", "value",
                         "sampleID", "fileName"))

    if (!"sampleID" %in% colnames(processedScreenData()))
      textOutput("'sampleID' should be defaulted selected")
    else
      selectInput("sampleAnnotations",
                  "Select sample information to display:",
                  choices = choices,
                  # selected = c("sampleID", "fileName"),
                  multiple = TRUE,
                  width = "100%")
  })

  # Show info Table for un-normalized data
  infoTableRaw <- reactive({

    req(processedScreenData())
    ## First panel: sample over view and subset
    ## A table to store unique samples information

    infoTable <- processedScreenData() %>%
      dplyr::select(c("sampleID", "fileName", input$sampleAnnotations)) %>%
      distinct(sampleID, .keep_all = TRUE)

    return(infoTable)
  })

  # # DT table for showing the sample annotations and filtering
  output$sampleInfo <- DT::renderDataTable({

    req(infoTableRaw())
    infoTable_new <- mutate_if(infoTableRaw(), is.character, as.factor)
    ## dplyr::select(-fileName)
    DT::datatable(infoTable_new,
                  filter = "top",
                  rownames = FALSE,
                  selection = "single",
                  style = "bootstrap")
  })

  output$sampleInfo_ui <- renderUI({

    req(infoTableRaw())
    div(class = "table-container table1",
        h3("Sample Overview"),
        DT::dataTableOutput("sampleInfo")
    )
  })

  #-----------------------------------------------------------------------------
  # 1.2 normalization section

  ifDrug2 <- reactive({
    req(processedScreenData())
    "drug2" %in% colnames(processedScreenData())
  })

  output$selWells <- renderUI({

    req(processedScreenData())
    # only two drug combination allowed
    if ("drug2" %in% colnames(processedScreenData())){
      allDrugs <-
        unique(paste0(processedScreenData()$drug, "_",
                      processedScreenData()$drug2))
    } else {
      allDrugs <- unique(processedScreenData()$drug)
    }

    # selectInput
    tagList(
      selectInput("negWell","Negative control wells: drug names",
                  choices = sort(allDrugs),
                  multiple = T,
                  selected = NULL),
      selectInput("posWell", "Positive wells: drug names",
                  choices = sort(allDrugs),
                  multiple = T,
                  selected = NULL)
    )
  })

  # define the well type information
  # screenData is the normalized data
  screenData <- reactiveVal()
  warningMessage <- reactiveVal() # To store warnings

  observeEvent(input$doNorm, {

    req(processedScreenData())
    warningMessage(NULL)  # Reset warning message

    if (is.null(input$negWell))
      negWs <- c()
    else
      negWs <- input$negWell

    if (is.null(input$posWell))
      posWs <- c()
    else
      posWs <- input$posWell

    if (ifDrug2()) {
      # Both drug_drug2 amd drug2_drug work
      combs <- paste0(processedScreenData()$drug, "_",
                      processedScreenData()$drug2)
      # screenData0, data before screenData
      screenData0 <- dplyr::mutate(processedScreenData(),
                                   wellType = case_when(combs %in% negWs ~ "neg",
                                                        combs %in% posWs ~ "pos",
                                                        .default = "sample"))
    } else {
      screenData0 <- dplyr::mutate(processedScreenData(),
                                   wellType = case_when(drug %in% negWs ~ "neg",
                                                        drug %in% posWs ~ "pos",
                                                        .default = "sample"))
    }

    # Normalize data
    if (input$method == "negatives" && is.null(input$negWell)) {
      warningMessage("Need negative drugs for normalization")
      return()
    }

    if (input$method == "NPI" && (is.null(input$negWell) ||
                                  is.null(input$posWell))) {
      warningMessage("Need negative and positive drugs for normalization")
      return()
    }

    showModal(modalDialog("Normalizing ...", footer = NULL))

    tryCatch({
      screenData0 <- normalizePlate(
        screenData0,
        method = input$method,
        discardLayer = input$discardLayer,
        bySample = FALSE
      )

      if (!ifDrug2()) {
        screenData0 <- summariseScreen(screenData0,
                                       method = c("average", "AUC", "IC50"))
      }

      screenData(screenData0)
    }, error = function(e) {
      warningMessage(paste("Error:", e$message))
    })

    removeModal()
    screenData(screenData0)
  })

  output$warningMessage <- renderText({
    warningMessage()
  })

  # output normalize data
  normData <- reactive({

    if(ifDrug2())
      addDurgCol <- c("drug", "drug2")
    else
      addDurgCol <- "drug"

    req(screenData())
    selRows <- input$sampleInfo_row_last_clicked
    selectedPlates <- as.character(infoTableRaw()[selRows, ] %>%
                                     pull(fileName))

    if(length(selectedPlates) == 0)
      selectedPlates <- infoTableRaw() %>% pull(fileName)

    selectedPlates <- unique(selectedPlates)

    normTable <- screenData() %>%
      dplyr::select(any_of(c("sampleID", "fileName", "wellID", "rowID", "colID",
                             addDurgCol, input$sampleAnnotations_norm, "value",
                             "normVal", "normVal.cor"))) %>%
      filter(fileName %in% selectedPlates)

    return(normTable)
  })

  # # DT table for showing the sample annotations and filtering
  output$normTable <- DT::renderDataTable({

    req(normData())
    normData_new <- mutate_if(normData(), is.character, as.factor) %>%
      mutate(across(starts_with("normVal"), ~ format(round(.x, 3), nsmall = 3)))

    DT::datatable(normData_new,
                  filter = "top",
                  rownames = FALSE,
                  selection = 'none',
                  style = "bootstrap")
  })

  output$normTable_ui <- renderUI({
    req(normData())
    div(class = "table-container table2",
        h3("Normalized Values"),
        DT::dataTableOutput("normTable")
    )
  })

  #-----------------------------------------------------------------------------
  # 1.3 adjust incubation effect

  output$estimateMethod <- renderUI({
    # if (input$normalization) {
    req(screenData())
    selectInput("estimateMethod",
                "Method to estimate the shape and strength of edge effect.",
                choices = c('loess', 'sigmoid'),
                selected = "loess")
  })

  output$useNeg <- renderUI({
    req(screenData())
    checkboxInput("useNeg",
                  "All or Negative wells used to estimate edge effect",
                  value = TRUE)
  })

  output$span <- renderUI({
    req(screenData())
    if (input$estimateMethod == "loess")
      numericInput("span", "Span parameter for loess", value = 1)
  })

  output$correctMethod <- renderUI({
    req(screenData())
    selectInput("correctMethod", "Methods to adjust edge effects",
                choices = c('bliss', 'linear'),
                selected = "loess")
  })

  observeEvent(input$doCorrEdge, {

    span <- ifelse(is.null(input$span), 1, input$span)
    showModal(modalDialog("Adjusting and summarizing ...", footer = NULL))


    screenData_fitted <-
      tryCatch({
        correctEdgeEffect(screenData(),
                          estimateMethod = input$estimateMethod,
                          useNeg = input$useNeg,
                          span = span,
                          correctMethod = input$correctMethod)},
        error = function(e) {
          message <- warningMessage(paste("Error:", e$message))
          list(message = message)
        })

    output$correctEdgeEffect_message <- renderText({
      req(screenData_fitted())
      if (!is.null(screenData_fitted()$message))
        screenData_fitted()$message
    })

    # resummarize
    # remove previous values

    rmCols <- c("meanViab.cor", "AUC.cor", "UpperLimit.cor", "IC50.cor",
                "Slope.cor", "LowerLimit.cor")

    colID <- unlist(sapply(rmCols, grep, colnames(screenData_fitted)))
    if (length(colID) > 0) {
      screenData_fitted <- screenData_fitted[,-colID]
    }

    screenData_fitted <- summariseScreen(screenData_fitted,
                                         method = c("average", "AUC", "IC50"))
    screenData(screenData_fitted)
    removeModal()

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
      showTab(inputId = "tabs", target = "Plate plot")
      showTab(inputId = "tabs", target = "Overview of Two-Drug Combination")
      showTab(inputId = "tabs", target = "Combination hypothesis test")
      showTab(inputId = "tabs", target = "Drug Combination Effects")
    } else {
      hideTab(inputId = "tabs", target = "Overview of Two-Drug Combination")
      hideTab(inputId = "tabs", target = "Combination hypothesis test")
      hideTab(inputId = "tabs", target = "Drug Combination Effects")
      showTab(inputId = "tabs", target = "Dose-response curves")
      showTab(inputId = "tabs", target = "Plate plot")
      showTab(inputId = "tabs", target = "Clustering")
      showTab(inputId = "tabs", target = "Association test")
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

    if (!is.null(input$ifCensor) && input$ifCensor) {
      if (input$ifNorm %in% c("viability", "Adjusted viability")) {
        upper <- 1.5
        lower <- 0
      } else if (input$ifNorm == "z-score") {
        upper <- 4
        lower <- 4
      }
    }

    tagList(div(style = "display:inline-block; width: 90px",
                textInput("lowerLimit", "lower limit", value = lower)),
            div(style = "display:inline-block; width: 90px",
                textInput("upperLimit", "upper limit", value = upper)))
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
  output$infoTab <- renderDataTable({

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
                       DrugScreenExplorer:::detectClass(
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
    typeMap <- structure(c("meanViab", "AUC", "IC50"),
                         names = c("average viability", "AUC", "IC50"))
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
      print(plotTab)
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

  # prepare the clustering plot on panel 3
  plotCluster <- eventReactive(input$doPlot, {

    # plotCluster <- reactive({
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

    # heatmap in ggplot function


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
        g <-
          pheatmap(viabMat,
                   color = colorRampPalette(brewer.pal(n = 7, name = "RdYlBu"))(100),
                   scale = "none",
                   annotation_col = annoCol,
                   cluster_rows = hr,
                   cluster_cols = hc,
                   silent = TRUE)
      } else {
        # if nothing to annotate
        g <-
          pheatmap(viabMat,
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

        g <- ggplot(pcaTab, aes_string(x = "PC1", y = "PC2", color = input$seleColor1)) +
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
    }

    return(g)
  })

  output$clusterPlot <- renderPlot({
    plotCluster()
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
      print(plotCluster())
      dev.off()
    }
  )

  ##############################################################################
  # Panel 5
  ##############################################################################

  # library(ComplexHeatmap)
  # library(circlize)
  # library(RColorBrewer)

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
        testTab <- testInput() %>%
          group_by(drug) %>%
          nest()

        if (colType == "binary") {
          testParam <- input$seleParam == "Equal variance"
          res <- mutate(testTab, model = map(data, ~ t.test(viab ~ status, ., var.equal = TRUE))) %>%
            mutate(out = map(model, broom::tidy)) %>%
            unnest(out) %>%
            ungroup() %>%
            mutate(diffMean = estimate2 - estimate1, adjusted.p = p.adjust(p.value, method = "BH")) %>%
            select(drug, diffMean, p.value, adjusted.p) %>%
            arrange(p.value)

        } else if (colType == "continuous") {
          testParam <- ifelse(input$seleParam != "Nonparametric (Spearman)", "pearson", "spearman")
          res <- mutate(testTab,
                        model = map(data, ~ cor.test( ~ viab + status, ., method = testParam))) %>%
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
      paste0("Please first select a feature for comparison in `sample overview`
             page: Select sample information to display")
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

  output$sumCombineTable <- DT::renderDataTable({
    req(sumCombTab())
    DT::datatable(sumCombTab(),
                  filter = "top",
                  rownames = FALSE,
                  selection = 'none',
                  style = "bootstrap")
  })

  output$combHypothesisControls <- renderUI({
    req(screenData_combine())
    availableMetrics <- c("CI" = "CI", "logCI" = "logCI",
                          "Corrected CI" = "CI.cor",
                          "Corrected logCI" = "logCI.cor")
    availableMetrics <- availableMetrics[
      unname(availableMetrics) %in% colnames(screenData_combine())]
    combinations <- screenData_combine() %>%
      distinct(drug, drug2) %>%
      mutate(combination = paste(drug, drug2, sep = " + ")) %>%
      arrange(combination)
    metadataChoices <- intersect(
      if (is.null(input$sampleAnnotations)) character(0) else input$sampleAnnotations,
      colnames(screenData_combine())
    )
    doses <- screenData_combine() %>%
      distinct(concentration, concentration2) %>%
      mutate(doseKey = paste(concentration, concentration2, sep = "\r"),
             doseLabel = paste0("Drug 1: ", concentration,
                                "; Drug 2: ", concentration2)) %>%
      arrange(concentration, concentration2)

    if (length(availableMetrics) == 0 || nrow(combinations) == 0 ||
        length(metadataChoices) == 0) {
      return(tags$p(
        "A CI column, one drug combination, and at least one metadata column are required."))
    }

    tagList(
      selectInput("combHypothesisMetric", "CI metric",
                  choices = availableMetrics,
                  selected = unname(availableMetrics)[1]),
      selectInput("combHypothesisTest", "Statistical test",
                  choices = c("Welch two-sample t-test" = "t",
                              "Wilcoxon rank-sum test" = "wilcox"),
                  selected = "t"),
      selectInput("combHypothesisCombination", "Drug combination",
                  choices = stats::setNames(combinations$combination,
                                            combinations$combination),
                  selected = combinations$combination[1]),
      selectInput("combHypothesisMetadata", "Metadata variable",
                  choices = metadataChoices,
                  selected = metadataChoices[1]),
      uiOutput("combHypothesisGroups"),
      selectInput("combHypothesisDose", "Concentration pair",
                  choices = c(
                    "All concentration pairs" = "all",
                    stats::setNames(doses$doseKey, doses$doseLabel)),
                  selected = "all"),
      tags$p("Tests compare CI distributions between metadata groups within the selected drug combination. ",
             "P-values are adjusted using Benjamini-Hochberg correction.")
    )
  })

  output$combHypothesisGroups <- renderUI({
    req(screenData_combine(), input$combHypothesisMetadata)
    metadataValues <- screenData_combine() %>%
      pull(.data[[input$combHypothesisMetadata]]) %>%
      as.character() %>%
      unique() %>%
      sort()
    selectInput("combHypothesisGroups", "Metadata groups to compare",
                choices = metadataValues, selected = metadataValues,
                multiple = TRUE, selectize = TRUE)
  })

  combHypothesisError <- reactiveVal(NULL)

  combHypothesisData <- reactive({
    req(screenData_combine(), input$combHypothesisMetric,
        input$combHypothesisCombination, input$combHypothesisMetadata,
        input$combHypothesisGroups, input$combHypothesisDose)
    metric <- input$combHypothesisMetric
    metadata <- input$combHypothesisMetadata
    data <- screenData_combine() %>%
      mutate(combination = paste(drug, drug2, sep = " + "),
             doseKey = paste(concentration, concentration2, sep = "\r"),
             value = .data[[metric]],
             groupValue = as.character(.data[[metadata]])) %>%
      filter(combination == input$combHypothesisCombination,
             groupValue %in% input$combHypothesisGroups,
             input$combHypothesisDose == "all" |
               doseKey == input$combHypothesisDose,
             !is.na(groupValue),
             is.finite(value)) %>%
      group_by(sampleID, groupValue) %>%
      summarise(value = mean(value), .groups = "drop") %>%
      mutate(combination = input$combHypothesisCombination)
    if (nrow(data) == 0)
      stop("No finite CI values are available for the selected combination, metadata groups, and concentration.")
    data
  })

  combHypothesisResults <- reactive({
    combHypothesisError(NULL)
    tryCatch({
      data <- combHypothesisData()
      groups <- sort(unique(data$groupValue))
      if (length(groups) < 2)
        stop("Select at least two metadata groups to compare.")
      if (any(table(data$groupValue) < 2))
        stop("Each selected metadata group needs at least two sample-level CI values.")

      results <- purrr::map_dfr(utils::combn(groups, 2, simplify = FALSE),
                                function(pair) {
        first <- data$value[data$groupValue == pair[1]]
        second <- data$value[data$groupValue == pair[2]]
        test <- if (input$combHypothesisTest == "t") {
          t.test(first, second, var.equal = FALSE)
        } else {
          wilcox.test(first, second, exact = FALSE)
        }
        tibble(
          group1 = pair[1], group2 = pair[2],
          n1 = length(first), n2 = length(second),
          estimate_difference = if (input$combHypothesisTest == "t")
            mean(first) - mean(second) else median(first) - median(second),
          p.value = test$p.value
        )
      })
      results %>%
        mutate(adjusted.p = p.adjust(p.value, method = "BH")) %>%
        arrange(p.value)
    }, error = function(e) {
      combHypothesisError(conditionMessage(e))
      tibble()
    })
  })

  output$combHypothesisError <- renderUI({
    if (!is.null(combHypothesisError()))
      div(style = "color: red;", combHypothesisError())
  })

  output$combHypothesisResults <- DT::renderDataTable({
    results <- combHypothesisResults()
    if (nrow(results) == 0)
      return(NULL)
    results %>%
      mutate(across(c(estimate_difference, p.value, adjusted.p),
                    ~ formatC(.x, digits = 3, format = "e"))) %>%
      DT::datatable(rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
  })

  output$combHypothesisPlot <- renderPlotly({
    data <- tryCatch(combHypothesisData(), error = function(e) NULL)
    if (is.null(data) || nrow(data) == 0)
      return(NULL)
    p <-     ggplot(data, aes(x = groupValue, y = value, fill = groupValue)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.5) +
      geom_jitter(width = 0.15, alpha = 0.7) +
      labs(x = input$combHypothesisMetadata, y = input$combHypothesisMetric,
           title = paste("Combination index by", input$combHypothesisMetadata),
           subtitle = input$combHypothesisCombination) +
      theme_bw() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 35, hjust = 1))
    ggplotly(p)
  })

  ##############################################################################
  # Panel 7, two-drug combination plot
  ##############################################################################

  seleDrug <- function(input, output, session, screenData_combine, drugB, drugC) {

    ns <- session$ns
    output$seleBaseDrugBox <- renderUI({
      req(is.data.frame(screenData_combine),
          "drug" %in% names(screenData_combine))
      selectedDrug <- drugB()
      baseDrug <- if (length(selectedDrug) == 1 &&
                      !is.na(selectedDrug) &&
                      selectedDrug %in% unique(screenData_combine$drug)) {
        selectedDrug
      } else {
        unique(screenData_combine$drug)[1]
      }
      selectInput(ns("seleBaseDrug"), "Base Drug",
                  unique(screenData_combine$drug),
                  selected = baseDrug)
    })

    output$seleCombiDrugBox <- renderUI({
      req(is.data.frame(screenData_combine),
          "drug2" %in% names(screenData_combine))
      selectedDrug <- drugC()
      combiDrug <- if (length(selectedDrug) == 1 &&
                       !is.na(selectedDrug) &&
                       selectedDrug %in% unique(screenData_combine$drug2)) {
        selectedDrug
      } else {
        unique(screenData_combine$drug2)[1]
      }
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

    req(screenData_combine(), length(selectedDrug$drugB) == 1,
        length(selectedDrug$drugC) == 1)
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

    req(screenData_combine(), length(selectedDrug$drugB) == 1,
        length(selectedDrug$drugC) == 1)
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

    req(screenData_combine(), length(selectedDrug$drugB) == 1,
        length(selectedDrug$drugC) == 1)
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

    rv_bayes$plot <- DrugScreenExplorer:::plot_synergyscreen(
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
}

shinyApp(ui = ui, server = server)
