#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output, session) {
  
  #do plots need customized titles?
  #fix selecting by target type and model type
    #this is hard...
  #fix smooth line
  #increase size of all plots when facetting
  #fix color/facet when all option selected
  #fix group by statement
  output$locationPlot <- renderPlotly({

     dat <- scores_adj %>%
      group_by_("Location", "Epiweek",
        ifelse(input$location_color != "None", input$location_color, "Location"),
        ifelse(input$location_facet != "None", input$location_facet, "Epiweek")) %>% 
     dplyr::summarize(
        avg_score = mean(score_adj),
        skill = exp(avg_score)) %>% 
      filter(Location == input$location)
     
      if (input$location_color == "None") {
        p <- ggplot(dat, aes(x = Epiweek, y = skill, group = 1)) + 
        geom_line(size = 1.25, alpha = 0.9)
      } else {
        p <- ggplot(dat, aes(x = Epiweek, y = skill)) + 
        geom_line(size = 1.25, alpha = 0.9)
      }
      
      if (input$location_color != "None") {
        p = p + aes_string(col = input$location_color, group = input$location_color)
      }
     
      if (input$location_smooth == "on_location") {
        p = p + geom_smooth(group=1)
      }

      if (input$location_facet != "None") {
        p = p + facet_grid(reformulate(".",input$location_facet))
      }
     
      ggplotly(p + labs(x = "Epiweek", y = "Model Skill"), 
        tooltip=c("x","y","colour"))
  })
  
  output$seasonPlot <- renderPlotly({
    
    dat <- scores_adj %>%
      group_by_("Season", "Epiweek",
                ifelse(input$season_color != "None", input$season_color, "Season"),
                ifelse(input$season_facet != "None", input$season_facet, "Epiweek")) %>% 
      dplyr::summarize(
        avg_score = mean(score_adj),
        skill = exp(avg_score)) %>% 
        filter(Season == input$season)
    
    if (input$season_color == "None") {
      p <- ggplot(dat, aes(x = Epiweek, y = skill, group = 1)) + 
        geom_line(size = 1.25, alpha = 0.9)
    } else {
      p <- ggplot(dat, aes(x = Epiweek, y = skill)) + 
        geom_line(size = 1.25, alpha = 0.9)
    }
    
    if (input$season_color != "None") {
      p = p + aes_string(col = input$season_color, group = input$season_color)
    }
    
    if (input$season_smooth == "on_season") {
      p = p + geom_smooth(group=1)
    }
    
    if (input$season_facet != "None") {
      p = p + facet_grid(reformulate(".",input$season_facet))
    }
    
    ggplotly(p + labs(x = "Epiweek", y = "Model Skill"),
             tooltip=c("x","y","colour"))
  })
  
  output$modelPlot <- renderPlotly({
    
    dat <- scores_adj %>%
      group_by_("Model", "Epiweek",
                ifelse(input$model_color != "None", input$model_color, "Model"),
                ifelse(input$model_facet != "None", input$model_facet, "Epiweek")) %>% 
      dplyr::summarize(
        avg_score = mean(score_adj),
        skill = exp(avg_score)) %>% 
        filter(Model == input$model)

    
    if (input$model_color == "None") {
      p <- ggplot(dat, aes(x = Epiweek, y = skill, group = 1)) + 
        geom_line(size = 1.25, alpha = 0.9)
    } else {
      p <- ggplot(dat, aes(x = Epiweek, y = skill)) + 
        geom_line(size = 1.25, alpha = 0.9)
    }
    
    if (input$model_color != "None") {
      p = p + aes_string(col = input$model_color, group = input$model_color)
    }
    
    if (input$model_smooth == "on_model") {
      p = p + geom_smooth(group=1)
    }
    
    if (input$model_facet != "None") {
      p = p + facet_grid(reformulate(".",input$model_facet))
    }
    
    ggplotly(p + labs(x = "Epiweek", y = "Model Skill"),
             tooltip=c("x","y","colour"))
  })
  
})
