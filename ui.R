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
        tags$style(type="text/css","label {display:inline;}")
      ),

      #side panel 1
      conditionalPanel(condition="input.tabselected==1",
                       helpText("Visualize FluSight Network model performance over the past 7 influenza seasons."),
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
                         label = h4("Model type highlight:"),
                         choices = heatmap_highlight),
                       helpText("This app was created by Evan R Moore and Nicholas G Reich at the University of Massachusetts-Amherst, in collaboration with the", a("FluSight Network.", href="https://github.com/reichlab/flusight"), "This work was funded in part by the U.S. National Institutes of Health MIDAS program (R35GM119582) and a DARPA Young Faculty Award (Dl6AP00144). The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institute Of General Medical Sciences, the National Institutes of Health, or the Defense Advanced Projects Research Agency."),
                       hr()),
      #side panel 2
      conditionalPanel(condition="input.tabselected==2",
                       helpText("The following tabs focus on by-epiweek performance."),
                       selectInput(
                         "location",
                         label = h3("What region do you want to see?"),
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
                       hr()),
      
    #side panel 3
    conditionalPanel(condition="input.tabselected==3",
                     helpText(""),
                     selectInput(
                       "season",
                       label = h3("What season do you want to see?"),
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
                     hr()),
    
    #side panel 4
    conditionalPanel(condition="input.tabselected==4",
                     helpText(""),
                     selectInput(
                       "model",
                       label = h3("What model do you want to see?"),
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
                     hr())),
    
    #conditional main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Overall Results", plotOutput("heatmapPlot"), value = 1,
                 conditionalPanel(condition = "input.tabselected==1")),
        tabPanel("Results By Location", plotlyOutput("locationPlot"), value = 2,  
                 conditionalPanel(condition="input.tabselected==2")),
        tabPanel("By Season", plotlyOutput("seasonPlot"), value = 3,
                 conditionalPanel(condition="input.tabselected==3")),
        tabPanel("By Model", plotlyOutput("modelPlot"), value = 4,  
                 conditionalPanel(condition="input.tabselected==4")),
        id = "tabselected")
    )
  )
)