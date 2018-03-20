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

setwd("~/reichlab/FSN_Model_Comparison")

scores <- read_csv("data/scores.csv")
models <- read_csv("data/model-id-map.csv")
complete_models <- c(models$`model-id`[models$complete=="true"], "UTAustin-edm")
compartment <- c("CU-EAKFC_SEIRS", "CU-EAKFC_SIRS", "CU-EKF-SEIRS","CU-EKF_SIRS",
                 "CU-RHF_SIRS","CU-RHF_SEIRS","LANL-DBM")

## define column with scores of interest
SCORE_COL <- quo(`Multi bin score`)

all_target_bounds <- read_csv("data/all-target-bounds.csv")

## Remove scores that fall outside of evaluation period for a given target/season
scores_trimmed <- scores %>%
  dplyr::left_join(all_target_bounds, by = c("Season", "Target", "Location")) %>%
  dplyr::filter(`Model Week` >= start_week_seq, `Model Week` <= end_week_seq)

scores_adj <- scores_trimmed %>%
  filter(Model %in% complete_models) %>%
  ## if NA, NaN or <-10, set score to -10
  mutate(score_adj = dplyr::if_else(is.nan(!!SCORE_COL) | is.na(!!SCORE_COL) , 
                                    -10, 
                                    !!SCORE_COL),
        `Target Type` = dplyr::if_else(Target %in% c("Season onset", "Season peak week", "Season peak percentage"),
                                      "seasonal", "k-week-ahead"),
        `Model Type` = ifelse(Model %in% compartment, "Compartmental", "Non Compartmental")) %>%
  mutate(score_adj = dplyr::if_else(score_adj < -10 , -10, score_adj)) 
scores_adj <- scores_adj %>% filter(!(Epiweek %in% c(41, 42, 53)))
scores_adj$Epiweek <- factor(scores_adj$Epiweek, levels = c(43:52, 1:18))

regions  <- unique(scores_adj$Location)
models <- complete_models
seasons <- unique(scores_adj$Season)
vars <- c("None", "Location", "Season", "Target", "Target Type", "Model", "Model Type")

ui <- shinyUI(
  pageWithSidebar(
    headerPanel("FluSight Network Model Comparison"),
    
    sidebarPanel(
      #side panel 1
      conditionalPanel(condition="input.tabselected==1",
                       helpText("This app allows you to visualize FluSight Network model performance over the past 7 influenza seasons, with a focus on by-epiweek performance. Created for the ReichLab by Evan Moore and Nicholas Reich."),
                       selectInput(
                         "location",
                         label = h3("Select HHS Region:"),
                         choices = regions),
                       selectInput(
                         "location_color",
                         label = h3("Select Color:"),
                         choices = vars[!(vars %in% "Location")]),
                       selectInput(
                         "location_facet",
                         label = h3("Select Facet:"),
                         choices = vars[!(vars %in% "Location")]),
                       radioButtons("location_smooth", "Toggle Smoothed Regression Line",
                                    c("On" = "on_location",
                                      "Off" = "off_location"),
                                    selected="off_location"),
                       hr()),
      
    #side panel 2
    conditionalPanel(condition="input.tabselected==2",
                     helpText(""),
                     selectInput(
                       "season",
                       label = h3("Select Season:"),
                       choices = seasons),
                     selectInput(
                       "season_color",
                       label = h3("Select Color:"),
                       choices = vars[!(vars %in% "Season")]),
                     selectInput(
                       "season_facet",
                       label = h3("Select Facet:"),
                       choices = vars[!(vars %in% "Season")]),
                     radioButtons("season_smooth", "Toggle Smoothed Regression Line",
                                  c("On" = "on_season",
                                    "Off" = "off_season"),
                                  selected="off_season"),
                     hr()),
    
    #side panel 3
    conditionalPanel(condition="input.tabselected==3",
                     helpText(""),
                     selectInput(
                       "model",
                       label = h3("Select Model:"),
                       choices = models),
                     selectInput(
                       "model_color",
                       label = h3("Select Color:"),
                       choices = vars[!(vars %in% "Model")]),
                     selectInput(
                       "model_facet",
                       label = h3("Select Facet:"),
                       choices = vars[!(vars %in% "Model")]),
                     radioButtons("model_smooth", "Toggle Smoothed Regression Line",
                                  c("On" = "on_model",
                                    "Off" = "off_model"),
                                  selected="off_model"),
                     hr())),
    
    #conditional main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Location", plotlyOutput("locationPlot"), value = 1,  
                 conditionalPanel(condition="input.tabselected==1")),
        tabPanel("Season", plotlyOutput("seasonPlot"), value = 2,
                 conditionalPanel(condition="input.tabselected==2")),
        tabPanel("Model", plotlyOutput("modelPlot"), value = 3,  
                 conditionalPanel(condition="input.tabselected==3")),
        id = "tabselected")
    )
  )
)