#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(readr)

source("scripts/load_data.R")

ui <- shinyUI(
  pageWithSidebar(
    headerPanel("FluSight Network Model Comparison"),
    
    sidebarPanel(
      tags$head(
        tags$style(type="text/css", 
                   "label.control-label, .selectize-control.single { 
         display: table-cell; 
         text-align: center; 
         vertical-align: middle; 
      } 
      label.control-label {
        padding-right: 5px;
      }
      .form-group { 
        display: table-row;
      }
      .selectize-control.single div.item {
        padding-right: 5px;
      }
                      .selectize-input {
                      white-space: nowrap;
                      padding: 25px;
                      height: 25px !important;
                      }
                      .selectize-dropdown {
                      width: 150px !important;
                      line-height: 25px; 
                      }'
              )
            )")
      ),
      #side panel 1
      conditionalPanel(condition="input.tabselected==1",
                       helpText("Visualize FluSight Network model performance over the past 7 influenza seasons."),
                       helpText("This app was created by Evan R Moore and Nicholas G Reich at the University of Massachusetts-Amherst, in collaboration with the FluSight Network. This work was funded in part by the U.S. National Institutes of Health MIDAS program (R35GM119582) and a DARPA Young Faculty Award (Dl6AP00144). The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institute Of General Medical Sciences, the National Institutes of Health, or the Defense Advanced Projects Research Agency."),
                       hr(),
                       helpText(a("FluSight Network Website", href = "http://flusightnetwork.io/")),
                       helpText(a("FluSight Network Model Comparison App on GitHub", href = "https://github.com/evanm31/FSN-Model-Comparison"))),
      #side panel 2
      conditionalPanel(condition="input.tabselected==2",
                       helpText("Visualize FluSight Network model performance over the past 7 influenza seasons."),
                       h3("Plot Elements"),
                       selectInput(
                         "heatmap_x",
                         label = h4("X-axis:"),
                         choices = heatmap_x),
                       selectInput(
                         "heatmap_facet",
                         label = h4("Facet:"),
                         choices = heatmap_fac),
                       selectInput(
                         "heatmap_highlight",
                         label = h4("Highlight:"),
                         choices = heatmap_highlight),
                       hr(),
                       helpText("This app was created by Evan R Moore and Nicholas G Reich at the University of Massachusetts-Amherst, in collaboration with the FluSight Network. This work was funded in part by the U.S. National Institutes of Health MIDAS program (R35GM119582) and a DARPA Young Faculty Award (Dl6AP00144). The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institute Of General Medical Sciences, the National Institutes of Health, or the Defense Advanced Projects Research Agency.")),
      #side panel 3
      conditionalPanel(condition="input.tabselected==3",
                       helpText("Visualize FluSight Network model performance over the past 7 influenza seasons."),
                       selectInput(
                         "location",
                         label = h4("What region?"),
                         choices = regions),
                       h3("Plot Elements"),
                       selectInput(
                         "location_color",
                         label = h4("Color:"),
                         choices = vars_col[!(vars_col %in% "Location")]),
                       selectInput(
                         "location_facet",
                         label = h4("Facet:"),
                         choices = vars_fac[!(vars_fac %in% "Location")]),
                       hr(),
                       helpText("This app was created by Evan R Moore and Nicholas G Reich at the University of Massachusetts-Amherst, in collaboration with the FluSight Network. This work was funded in part by the U.S. National Institutes of Health MIDAS program (R35GM119582) and a DARPA Young Faculty Award (Dl6AP00144). The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institute Of General Medical Sciences, the National Institutes of Health, or the Defense Advanced Projects Research Agency.")),
      
    #side panel 4
    conditionalPanel(condition="input.tabselected==4",
                     helpText("Visualize FluSight Network model performance over the past 7 influenza seasons."),
                     selectInput(
                       "season",
                       label = h4("What season?"),
                       choices = seasons),
                     h3("Plot Elements"),
                     selectInput(
                       "season_color",
                       label = h4("Color:"),
                       choices = vars_col[!(vars_col %in% "Season")]),
                     selectInput(
                       "season_facet",
                       label = h4("Facet:"),
                       choices = vars_fac[!(vars_fac %in% "Season")]),
                     hr(),
                     helpText("This app was created by Evan R Moore and Nicholas G Reich at the University of Massachusetts-Amherst, in collaboration with the FluSight Network. This work was funded in part by the U.S. National Institutes of Health MIDAS program (R35GM119582) and a DARPA Young Faculty Award (Dl6AP00144). The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institute Of General Medical Sciences, the National Institutes of Health, or the Defense Advanced Projects Research Agency.")),
    
    #side panel 5
    conditionalPanel(condition="input.tabselected==5",
                     helpText("Visualize FluSight Network model performance over the past 7 influenza seasons."),
                     selectInput(
                       "model",
                       label = h4("What model?"),
                       choices = models),
                     h3("Plot Elements"),
                     selectInput(
                       "model_color",
                       label = h4("Color:"),
                       choices = vars_col[!(vars_col %in% c("Model", "Model_Type"))]),
                     selectInput(
                       "model_facet",
                       label = h4("Facet:"),
                       choices = vars_fac[!(vars_fac %in% c("Model", "Model_Type"))]),
                     hr(),
                     helpText("This app was created by Evan R Moore and Nicholas G Reich at the University of Massachusetts-Amherst, in collaboration with the FluSight Network. This work was funded in part by the U.S. National Institutes of Health MIDAS program (R35GM119582) and a DARPA Young Faculty Award (Dl6AP00144). The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institute Of General Medical Sciences, the National Institutes of Health, or the Defense Advanced Projects Research Agency."))),
    
    #conditional main panel
    mainPanel(
      tabsetPanel(
        tabPanel("About", value = 1,
                 helpText("The FluSight Network is a collaborative consortium of scientists and researchers participating in the CDC's annual \"Forecast the Influenza Season Collaborative Challenge\" (a.k.a. FluSight). This effort has galvanized a community interested in infectious disease forecasting, creating an organic testbed for improving both our technical understanding of how different forecast models perform but also how to integrate these models into decision-making. For more information on the participants and efforts made in ensemble forecasting view this", a(href="http://reichlab.io/2017/11/28/flusight-ensemble.html", "blog post"), "on the ReichLab website."),
                 helpText("The FluSight challenge focuses on forecasts of the weighted percentage of doctor's office visits for influenza-like-illness (wILI) in a particular region. The FluSight challenges have defined seven forecasting targets of particular public health relevance. Three of these targets are fixed scalar values for a particular season: onset week, peak week, and peak intensity (i.e. the maximum observed wILI percentage). The remaining four targets are the observed wILI percentages in each of the subsequent four weeks."),
                 helpText("Influenza forecasts have been evaluated by the CDC primarily using the log-score. While log scores are not on a particularly interpretable scale, exponentiating an average log score yields a forecast score equivalent to the geometric mean of the probabilities assigned to the eventually observed outcome. In this setting, this score has the intuitive interpretation of being the average probability assigned to the true outcome (where average is considered to be a geometric average). In this app, we will refer to skill as an exponentiated average log score"),
                 img(src = "timezero.png", width = "875px", height = "350px")),
        tabPanel("Overall Results", plotOutput("heatmapPlot"), value = 2,
                 conditionalPanel(condition = "input.tabselected==2")),
        tabPanel("Results By Location", plotlyOutput("locationPlot"), value = 3,  
                 conditionalPanel(condition="input.tabselected==3")),
        tabPanel("By Season", plotlyOutput("seasonPlot"), value = 4,
                 conditionalPanel(condition="input.tabselected==4")),
        tabPanel("By Model", plotlyOutput("modelPlot"), value = 5,  
                 conditionalPanel(condition="input.tabselected==5")),
        id = "tabselected")
    )
  )
)