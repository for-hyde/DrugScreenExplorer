
seleDrugUI <- function(id) {
  ns <- NS(id)
  tagList(tags$h4("Drug selection", style = "color: #2166AC"),
          uiOutput(ns("seleBaseDrugBox")),
          uiOutput(ns("seleCombiDrugBox")))
}


# define modules
# library(shinyr)
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
                         "Separator Character",
                         value = ","),
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


  # Panel 6
  tabPanel("Drug Combination Effects",
           titlePanel("Overview of drug combination"),

           sidebarLayout(
             sidebarPanel(uiOutput("negDrugBox"),
                          uiOutput("synMethodBox")),
             mainPanel(
               tags$style(type = "text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"),
               DT::dataTableOutput("sumCombineTable"))
           )),

  # Panel 7
  tabPanel("Combination Effects: Dose-response Plot",
           titlePanel("Combination Effects: Dose-response Plot"),

           sidebarLayout(
             sidebarPanel(
               seleDrugUI("doseDrugPair"),
               selectInput("plotType",
                           "Results",
                           c("Dose Response Curve",
                             "ScatterPlot: observe VS expected",
                             "Boxplot: CI by concentrations"))
             ),

             mainPanel(
               conditionalPanel(condition = "input.plotType == 'Dose Response Curve'",
                                plotlyOutput("doseCurvPlot", width = 800, height = 600)),

               conditionalPanel(condition = "input.plotType == 'ScatterPlot: observe VS expected'",
                                plotlyOutput("scatterPlot", width = 800, height = 800)),

               conditionalPanel(condition = "input.plotType == 'Boxplot: CI by concentrations'",
                                plotlyOutput("ciBoxplot", width = 600, height = 600))
               )
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
                numericInput("rowTop", "Top", value = 7,
                             min = 0,
                             max = 9999,
                             step = 1)),
            div(style = "display:inline-block; width: 90px",
                numericInput("rowBottom", "Bottom", value = 22,
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

    # print(processedScreenData)
  })


  ifDrug2 <- reactive({
    req(processedScreenData())
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

      if (!ifDrug2())
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
  # Panel 6, two drug combination
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
    selectInput("synMethod",
                "Method for synergy effects",
                c("bliss", "bayesynergy"),
                selected = "bliss",
                selectize = TRUE,
                multiple = TRUE)
  })

  screenData_combine <- reactive({

    req(screenData())
    if (ifDrug2()){
      screenData_combine <- summariseScreenCombination(screenData(),
                                                       negDrug = input$negDrug,
                                                       method = input$synMethod)
    } else {
    screenData_combine <- screenData()
    }

    return(screenData_combine)
  })

  sumCombTab <- reactive({

    req(screenData_combine())
    req(input$sampleAnnotations)
    inCol <- c("drug", "drug2", "concentration", "concentration2")
    if ("bliss" %in% input$synMethod) {
      inCol <- c(inCol, c("viab", "viab.cor",
                 "viabExp", "viab2.cor",
                 "CI", "logCI", "CI.cor", "logCI.cor"))
    }

    if ("bayesynergy" %in% input$synMethod) {
      inCol <- c(inCol, c("synergyScore", "synergyScore.cor",
                          "antagonismScore", "antagonismScore.cor"))
    }

    inCol <- intersect(inCol, colnames(screenData_combine()))
    inCol <- setdiff(inCol, "fileName")

    sumScreenCombine <- screenData_combine() %>%
      dplyr::select(c(input$sampleAnnotations, inCol)) %>%
      mutate_if(is.numeric, round, digits = 2)

    return(sumScreenCombine)
  })

  output$sumCombineTable <- DT::renderDataTable({
    req(sumCombTab())
    print(sumCombTab())
    DT::datatable(sumCombTab(),
                  filter = "top",
                  rownames = FALSE,
                  selection = 'none',
                  style = "bootstrap")
  })

  ##############################################################################
  # Panel 7, syn lines
  ##############################################################################
  #
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
      combiDrug <- ifelse(is.null(drugC()), unique(screenData_combine$drug2)[1],
                          drugC())
      selectInput(ns("seleCombiDrug"),
                  "Combined Drug",
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

  # Dose curve Plot

  output$doseCurvPlot <- renderPlotly({

    drugB <- selectedDrug$drugB
    drugC <- selectedDrug$drugC

    plotDat <- screenData_combine() %>%
      filter(drug == drugB, drug2 == drugC) %>%
      dplyr::distinct(sampleID, drug, concentration, drug2, concentration2, .keep_all = TRUE) %>%
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
                                       "Expected effect (A*B)")))

    g <- ggplot(data = plotDat,
                aes(x = concentration,
                    y = Viability,
                    col = Group,
                    group = Group,
                    linetype = Group)) +
      stat_summary(fun.data = "mean_se",
                   fun.args = list(mult = 2),
                   geom = "errorbar",
                   width = 0.05) +
      stat_summary(fun.y = "mean",
                   geom ="line", fun.args = list(mult = 2)) +
      theme_bw(base_size = 20) +
      theme(legend.title = element_blank(),
            legend.position = "top") +
      # guides(row = guide_legend(nrow = 1),
      #        linetype = guide_legend(nrow = 1)) +
      ylim(c(0, 1.5)) +
      xlab(paste0("Concentration of ", drugB)) +
      scale_color_manual(values = brewer.pal(name ="Set1", 9)[c(2, 3, 5, 4)]) +
      scale_linetype_manual(values = c(rep("solid", 3), "dashed"))

    ggplotly(g) %>%
      layout(legend = list(orientation = 'h',
                           x = 0.5, xanchor = 'center',
                           y = 1, yanchor = 'bottom',
                           ncol = 2,
                           title = "",
                           font = list(size = 18))) %>%
      config(displayModeBar = FALSE)
  })


  # Scatter plot
  output$scatterPlot <- renderPlotly({

    drugB <- selectedDrug$drugB
    drugC <- selectedDrug$drugC

    plotDat <- screenData_combine() %>%
      filter(drug == drugB, drug2 == drugC) %>%
      dplyr::distinct(sampleID, drug, concentration, drug2, concentration2, .keep_all = TRUE) %>%
      select(sampleID, concentration, concentration2, viabObs, viabExp)

    range = c(0, max(c(plotDat$viabObs, plotDat$viabExp)))

    g2 <- ggplot(plotDat,
                 aes(x = viabObs, y = viabExp,
                     color = sampleID)) +
      geom_point(alpha = 0.7)+
      geom_hline(aes(yintercept = 1), colour="grey", linetype = "dashed") +
      geom_vline(aes(xintercept = 1), colour="grey", linetype = "dashed") +
      # scale_color_manual() +
      facet_wrap( ~ concentration, ncol = 2) +
      geom_abline(intercept = 0, slope = 1, colour="black", linetype = "solid") +
      coord_fixed() +
      scale_x_continuous(limits = range) +
      scale_y_continuous(limits = range) +
      theme_bw(base_size = 12) +
      ylab(paste0("Measured viability")) +
      xlab(paste0("Expected viability")) +
      ggtitle(paste(drugB, "and", drugC)) +
      annotate("text", x = 0.8 * max(range), y = 0.3, label= "synergy", size = 4, alpha = 0.4) +
      guides(col = FALSE)

    ggplotly(g2) %>%
      config(displayModeBar = FALSE)

  })

  # Box plot

  output$ciBoxplot <- renderPlotly({

    plotDat <- screenData_combine() %>%
      filter(drug == drugB, drug2 == drugC) %>%
      dplyr::distinct(sampleID, drug, concentration, drug2, concentration2, .keep_all = TRUE) %>%
      select(sampleID, concentration, concentration2, CI, logCI) %>%
      mutate(concentration = factor(concentration))

    g3 <- ggplot(plotDat, aes(x = concentration, y = CI)) +
      ggbeeswarm::geom_beeswarm(col = "#4DAF4A", alpha = 0.5) +
      geom_boxplot(outlier.shape = NA, alpha = 0.2, width = 0.3, color = "black") +
      geom_hline(yintercept = c(-1, 0, 1), lty = "dashed") +
      theme_bw(base_size = 15)  +
      xlab(paste0("Concentration of ", drugB)) +
      ylab(paste0("Combination index"))

    ggplotly(g3) %>%
      config(displayModeBar = FALSE)
  })


}

shinyApp(ui = ui, server = server)




