
seleDrugUI <- function(id) {
  ns <- NS(id)
  tagList(tags$h4("Select drugs", style = "color: #2166AC"),
          uiOutput(ns("seleBaseDrugBox")),
          uiOutput(ns("seleCombiDrugBox")))
}


#' Main Application UI for DrugScreenExplorer
#'
#' Creates the complete Shiny user interface for the DrugScreenExplorer application.
#' This function builds a multi-tab navigation interface for drug screening data analysis.
#'
#' @param ... additinal argument
#'
#'
#' @return A Shiny UI object (typically `shiny.tag` or `shiny.tag.list`) containing the application layout.
#' @import shiny
#' @importFrom shinyBS bsTooltip
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
#' @importFrom shiny tagAppendAttributes

.buildUI <- function(...) {

  shiny::navbarPage(

    "DrugScreenExplorer",
    inverse = TRUE,
    id = "tabs",

    tabPanel("Data Preprocessing",
             titlePanel("Data Preprocessing"),
             sidebarLayout(

               sidebarPanel(

                 shiny::tags$div(

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
                 uiOutput("normTable_ui"),
                 uiOutput("downloadUI") #Generate report

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
                         column(DTOutput("infoTab"), width = 4),
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

               mainPanel(DTOutput("testRes"),
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
                 DT::DTOutput("sumCombineTable"),

                 conditionalPanel(
                   condition = "input.synMethod == 'bliss' & input.CIType == 'CI'",
                   tags$div(style = "margin-top:10px; font-style: italic; color: gray;",
                            "Note: CI = viabObs - viabExp; viabOb, observed viability; viabExp, expected viability")),

                 conditionalPanel(
                   condition = "input.synMethod == 'bliss' & input.CIType == 'logCI'",
                   tags$div(style = "margin-top:10px; font-style: italic; color: gray;",
                            "Note: logCI = log10(viabObs / viabExp); viabObs, observed viability; viabExp, expected viability")))
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
}



