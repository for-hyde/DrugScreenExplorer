

library(shiny)
library(tidyverse)
library(pheatmap)
library(RColorBrewer)
library(plotly)
library(dr4pl)
library(DrugScreenExplorer)
library(DT)


load("./shinyData.RData")

ui <- navbarPage(

  "DrugScreenExplorer", inverse = TRUE, id = "tabs",

  tabPanel("Sample overview",

           titlePanel("Overview of samples in the screen"),
           dataTableOutput("sampleInfo")),

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
               uiOutput("ifCorCheck"), width =4),
             mainPanel(tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"),
                       plotlyOutput("plateHeatmap", width = 700, height = 400),
                       column(dataTableOutput("infoTab"),width = 4),
                       column(plotOutput("linePlot",
                                         width = 400, height = 300),
                              width = 4),
                       width = 8))),

  tabPanel("Dose-response curves",

           titlePanel("Plot dose-response curves for selected drugs and samples"),
           sidebarLayout(
             sidebarPanel(
               uiOutput("nameDrugBox"),
               uiOutput("seleGroup"),
               conditionalPanel(condition = "input.selectGroup != 'All samples'",
                                uiOutput("seleFeature")),
               uiOutput("colorBox"),
               uiOutput("ifCorCheck1"),
               checkboxInput("ifIC50", "Fit IC50 curve", value = FALSE)
             ),
             mainPanel(
               tags$style(type = "text/css",
                          ".shiny-output-error { visibility: hidden; }",
                          ".shiny-output-error:before { visibility: hidden; }"),
               plotlyOutput("curvePlot",width = 600, height= 400)
             ))),

  tabPanel("Clustering",

           titlePanel("Clustering samples or drugs based on drug response"),
           sidebarLayout(
             sidebarPanel(uiOutput("seleGroup1"),
                          conditionalPanel(condition = "input.selectGroup1 != 'All samples'"
                                           ,uiOutput("seleFeature1")),
                          uiOutput("sumTypeBox"),
                          radioButtons("plotType","Plot type", c("Heatmap","PCA","t-SNE","Drug-drug correlation"),
                                       selected = "Heatmap"),
                          conditionalPanel(condition = "input.plotType == 't-SNE'",
                                           numericInput("perplex", "Perplexity", value =20),
                                           numericInput("Niter", "Iteration round", value = 1000)),
                          conditionalPanel(condition = "input.plotType == 'Heatmap'",
                                           radioButtons("ifRowNorm","Normalization", c("None","Row","Column"), selected = "None")),
                          uiOutput("ifCorCheck2"),
                          uiOutput("colorBox1"),
                          checkboxInput("ifCensor1","Fix value range", value = FALSE),
                          uiOutput("censorLimitBox1"),
                          sliderInput("topPercent", "Top variant drugs", min = 10, max = 100, value = 100, step = 10),
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
  tabPanel("Association test",
           titlePanel("Test associations between drug responses and sample annotations"),
           sidebarLayout(sidebarPanel(uiOutput("seleGroup2"),
                                      conditionalPanel(condition = "input.selectGroup2 != 'All samples'",
                                                       uiOutput("seleFeature2")),
                                      uiOutput("sumTypeBox1"),
                                      uiOutput("factorBox"),
                                      textOutput("methodBox"),
                                      uiOutput("paramBox"),
                                      uiOutput("ifCorCheck3")),
                         mainPanel(dataTableOutput("testRes"),
                                   plotlyOutput("diffPlot", width=500, height = 500))
           )))




