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
      #side panel 1
      conditionalPanel(condition="input.tabselected==1",
                       helpText("This app allows you to visualize FluSight Network model performance over the past 7 influenza seasons, with a focus on by-epiweek performance. Created for the ReichLab by Evan Moore and Nicholas Reich."),
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
      
    #side panel 2
    conditionalPanel(condition="input.tabselected==2",
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
    
    #side panel 3
    conditionalPanel(condition="input.tabselected==3",
                     helpText(""),
                     selectInput(
                       "model",
                       label = h3("What model do you want to see?"),
                       choices = models),
                     h3("Plot Elements"),
                     selectInput(
                       "model_color",
                       label = h4("Color:"),
                       choices = vars_col[!(vars_col %in% "Model")]),
                     selectInput(
                       "model_facet",
                       label = h4("Facet:"),
                       choices = vars_fac[!(vars_fac %in% "Model")]),
                     hr())),
    
    #conditional main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Results By Location", plotlyOutput("locationPlot"), value = 1,  
                 conditionalPanel(condition="input.tabselected==1")),
        tabPanel("By Season", plotlyOutput("seasonPlot"), value = 2,
                 conditionalPanel(condition="input.tabselected==2")),
        tabPanel("By Model", plotlyOutput("modelPlot"), value = 3,  
                 conditionalPanel(condition="input.tabselected==3")),
        id = "tabselected")
    )
  )
)