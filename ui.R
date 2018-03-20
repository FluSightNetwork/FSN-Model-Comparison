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
                         label = h3("What region would you like to see?"),
                         choices = regions),
                       helpText("Plot Parameters"),
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
                       label = h3("What season would you like to see?"),
                       choices = seasons),
                     helpText("Plot Parameters"),
                     selectInput(
                       "season_color",
                       label = h3("Color:"),
                       choices = vars_col[!(vars_col %in% "Season")]),
                     selectInput(
                       "season_facet",
                       label = h3("Facet:"),
                       choices = vars_fac[!(vars_fac %in% "Season")]),
                     hr()),
    
    #side panel 3
    conditionalPanel(condition="input.tabselected==3",
                     helpText(""),
                     selectInput(
                       "model",
                       label = h3("What model would you like to see?"),
                       choices = models),
                     helpText("Plot Parameters"),
                     selectInput(
                       "model_color",
                       label = h3("Color:"),
                       choices = vars_col[!(vars_col %in% "Model")]),
                     selectInput(
                       "model_facet",
                       label = h3("Facet:"),
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

#what region do you want to see?
#header for graphical params but better
#then color and facet