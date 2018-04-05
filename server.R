library(shiny)

compartment <- c("CU-EAKFC_SEIRS", "CU-EAKFC_SIRS", "CU-EKF_SEIRS","CU-EKF_SIRS",
                 "CU-RHF_SIRS","CU-RHF_SEIRS","LANL-DBM")
backfill <- c("LANL-DBM")

shinyServer(function(input, output, session) {
  
  output$heatmapPlot <- renderPlot({
    dat <- scores_adj %>%
      group_by_("Model",
                input$heatmap_x,
                ifelse(input$heatmap_facet != "None", input$heatmap_facet, "Model")) %>% 
      summarize(
        avg_score = mean(score_adj),
        Skill = exp(avg_score),
        min_score = min(score_adj)
      ) %>%
      ungroup() %>%
      mutate(Model = reorder(Model, avg_score))
    
    midpt <- mean(filter(dat, Model=="ReichLab-KDE")$Skill)
    p <- ggplot(dat, 
                aes_string(x=input$heatmap_x, fill="Skill", y="Model")) + 
      geom_tile() + ylab(NULL) + xlab(NULL) +
      geom_text(aes(label=round(Skill, 2))) +
      scale_fill_gradient2(midpoint = midpt) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    
    if (input$heatmap_highlight != "None"){
      if (input$heatmap_highlight == "Compartmental"){
        p <- p + theme(axis.text.y=element_text(face=highlight(dat$Model, compartment, "bold"), color = highlight(dat$Model, compartment, "col")))
      } else {
        p <- p + theme(axis.text.y=element_text(face=highlight(dat$Model, backfill, "bold"), color = highlight(dat$Model, backfill, "col")))
      }
    }
    if (input$heatmap_facet != "None"){
      if (input$heatmap_facet == "Target_Type" & input$heatmap_x == "Target") {
        p <- p + facet_grid(reformulate(input$heatmap_facet,"."))   
      } else {
      p <- p + facet_grid(reformulate(".",input$heatmap_facet)) 
    }
    }
    p
  }, height = 600, width = 600)
  
  output$locationPlot <- renderPlotly({
    
    if (input$location != "All Regions"){
     dat <- scores_adj %>%
      group_by_("Location", "Epiweek",
        ifelse(input$location_color != "None", input$location_color, "Location"),
        ifelse(input$location_facet != "None", input$location_facet, "Epiweek")) %>% 
     dplyr::summarize(
        avg_score = mean(score_adj),
        Skill = exp(avg_score)) %>% 
      filter(Location == input$location) %>% 
       na.omit()
    } else {
      dat <- all_location %>%
        group_by_("Epiweek",
                  ifelse(input$location_color != "None", input$location_color, "Epiweek"),
                  ifelse(input$location_facet != "None", input$location_facet, "Epiweek")) %>% 
        dplyr::summarize(
          Skill = mean(exp(avg_score))) %>% 
        na.omit()
    }
     
     if (input$location_color == "None") {
       p <- ggplot(dat, aes(x = Epiweek, y = Skill, group = 1)) + 
         geom_line(size = 1.1, alpha = 0.9)
     } else {
       p <- ggplot(dat, aes(x = Epiweek, y = Skill)) + 
         geom_line(size = 1.1, alpha = 0.9)
     }

     if (input$location_color != "None") {
       p = p + aes_string(col = input$location_color, group = input$location_color)
     }
     
      if (input$location_facet != "None") {
        p = p + facet_wrap(as.formula(paste("~", input$location_facet)))
        if (!(input$location_facet %in% c("Target_Type", "Model_Type"))){
          ggplotly(p + labs(x = "Epiweek", y = "Model Skill") + scale_x_discrete(breaks = c(seq(43, 52, by = 2), seq(1, 18, by = 2))), tooltip=c("x","y","colour")) %>% 
            layout(height = 800, autosize=TRUE, margin = list(l = 60, b = 90))
        } else {
          ggplotly(p + labs(x = "Epiweek", y = "Model Skill") + scale_x_discrete(breaks = c(seq(43, 52, by = 2), seq(1, 18, by = 2))), tooltip=c("x","y","colour")) %>% 
            layout(height = 550, autosize = TRUE, margin = list(l = 60, b = 90))
        }
      } else {
        ggplotly(p + labs(x = "Epiweek", y = "Model Skill"), tooltip=c("x","y","colour")) %>% 
             layout(height = 550, autosize = TRUE)
      }
  })
  
  output$seasonPlot <- renderPlotly({
    
    if (input$season != "All Seasons"){
    dat <- scores_adj %>%
      group_by_("Season", "Epiweek",
                ifelse(input$season_color != "None", input$season_color, "Season"),
                ifelse(input$season_facet != "None", input$season_facet, "Epiweek")) %>% 
      dplyr::summarize(
        avg_score = mean(score_adj),
        Skill = exp(avg_score)) %>% 
        filter(Season == input$season) %>% 
      na.omit()
    } else {
      dat <- all_season %>%
        group_by_("Epiweek",
                  ifelse(input$season_color != "None", input$season_color, "Epiweek"),
                  ifelse(input$season_facet != "None", input$season_facet, "Epiweek")) %>% 
        dplyr::summarize(
          Skill = mean(exp(avg_score))) %>% 
        na.omit()
    }
    
    if (input$season_color == "None") {
      p <- ggplot(dat, aes(x = Epiweek, y = Skill, group = 1)) + 
        geom_line(size = 1.1, alpha = 0.9)
    } else {
      p <- ggplot(dat, aes(x = Epiweek, y = Skill)) + 
        geom_line(size = 1.1, alpha = 0.9)
    }
    
    if (input$season_color != "None") {
      p = p + aes_string(col = input$season_color, group = input$season_color)
    }
    
    if (input$season_facet != "None") {
      p = p + facet_wrap(as.formula(paste("~", input$season_facet)))
      if (!(input$season_facet %in% c("Target_Type", "Model_Type"))){
        ggplotly(p + labs(x = "Epiweek", y = "Model Skill") + scale_x_discrete(breaks = c(seq(43, 52, by = 2), seq(1, 18, by = 2))), tooltip=c("x","y","colour")) %>% 
          layout(height = 800, autosize=TRUE, margin = list(l = 60, b = 90))
      } else {
        ggplotly(p + labs(x = "Epiweek", y = "Model Skill") + scale_x_discrete(breaks = c(seq(43, 52, by = 2), seq(1, 18, by = 2))), tooltip=c("x","y","colour")) %>% 
          layout(height = 600, autosize = TRUE, margin = list(l = 60, b = 90))
      }
    } else {
      ggplotly(p + labs(x = "Epiweek", y = "Model Skill"), tooltip=c("x","y","colour")) %>% 
        layout(height = 600, autosize = TRUE)
    }
  })
  
  output$modelPlot <- renderPlotly({
    
  if (input$model != "All Models"){  
    dat <- scores_adj %>%
      group_by_("Model", "Epiweek",
                ifelse(input$model_color != "None", input$model_color, "Model"),
                ifelse(input$model_facet != "None", input$model_facet, "Epiweek")) %>% 
      dplyr::summarize(
        avg_score = mean(score_adj),
        Skill = exp(avg_score)) %>% 
        filter(Model == input$model) %>% 
      na.omit()
  } else {
    dat <- all_model %>%
      group_by_("Epiweek",
                ifelse(input$model_color != "None", input$model_color, "Epiweek"),
                ifelse(input$model_facet != "None", input$model_facet, "Epiweek")) %>% 
      dplyr::summarize(
        Skill = mean(exp(avg_score))) %>% 
      na.omit()
  }

    if (input$model_color == "None") {
      p <- ggplot(dat, aes(x = Epiweek, y = Skill, group = 1)) + 
        geom_line(size = 1.05, alpha = 0.9)
    } else {
      p <- ggplot(dat, aes(x = Epiweek, y = Skill)) + 
        geom_line(size = 1.05, alpha = 0.9)
    }
    
    if (input$model_color != "None") {
      p = p + aes_string(col = input$model_color, group = input$model_color)
    }
    
    if (input$model_facet != "None") {
      p = p + facet_wrap(as.formula(paste("~", input$model_facet)))
      if (!(input$model_facet %in% c("Target_Type", "Model_Type"))){
        ggplotly(p + labs(x = "Epiweek", y = "Model Skill") + scale_x_discrete(breaks = c(seq(43, 52, by = 2), seq(1, 18, by = 2))), tooltip=c("x","y","colour")) %>% 
          layout(height = 800, autosize=TRUE, margin = list(l = 60, b = 90))
      } else {
        ggplotly(p + labs(x = "Epiweek", y = "Model Skill") + scale_x_discrete(breaks = c(seq(43, 52, by = 2), seq(1, 18, by = 2))), tooltip=c("x","y","colour")) %>% 
          layout(height = 550, autosize = TRUE, margin = list(l = 60, b = 90))
      }
    } else {
      ggplotly(p + labs(x = "Epiweek", y = "Model Skill"), tooltip=c("x","y","colour")) %>% 
        layout(height = 550, autosize = TRUE)
    }
  })
  
})
