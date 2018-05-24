library(shiny)

theme_set(theme_bw())

shinyServer(function(input, output, session) {
  
  output$heatmapPlot <- renderPlot({
    
    dat <- scores_adj %>%
      group_by_("Model",
                input$heatmap_x,
                ifelse(input$heatmap_facet != "None", input$heatmap_facet, "Model")) %>%
      summarise(
        avg_score = mean(score_adj),
        Skill = exp(avg_score),
        min_score = min(score_adj)
      ) %>%
      ungroup() %>%
      mutate(Model = reorder(Model, avg_score))

    ## these lines create a new column with the 'baseline' model score
    dat <- dat %>%   
    group_by_(input$heatmap_x,
                ifelse(input$heatmap_facet != "None", input$heatmap_facet, input$heatmap_x)) %>%
      mutate(
        baseline_score = avg_score[Model=="ReichLab-KDE"]
      ) %>%
      ungroup() %>%
      ## these lines then create the skill for the baseline model and a "% skill change over baseline" column
      mutate(
        baseline_skill = exp(baseline_score),
        pct_diff_baseline_skill = ((Skill - baseline_skill)/baseline_skill) * 100
      )
    
    specify_decimal <- function(x, k=0) trimws(format(round(x, k), nsmall=k))
    
    p <- ggplot(dat, aes_string(x=input$heatmap_x, y="Model", fill="pct_diff_baseline_skill")) +
      geom_tile() + ylab(NULL) + xlab(NULL) +
      geom_text(aes(label=specify_decimal(Skill, 2))) +
      scale_fill_gradient2(name = "% change \nfrom baseline") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    if (input$heatmap_highlight != "None"){
      if (input$heatmap_highlight == "Compartmental"){
        p <- p + theme(axis.text.y=element_text(face=highlight(dat$Model, compartment, "bold"), color = highlight(dat$Model, compartment, "col")))
      } else if (input$heatmap_highlight == "Backfill") {
        p <- p + theme(axis.text.y=element_text(face=highlight(dat$Model, backfill, "bold"), color = highlight(dat$Model, backfill, "col")))
      } else {
        p <- p + theme(axis.text.y=element_text(face=highlight(dat$Model, ensemble, "bold"), color = highlight(dat$Model, ensemble, "col")))
      }
    } else {
      p <- p + theme(axis.text.y = element_text(color = "black"))
    }
    if (input$heatmap_facet != "None"){
      if (input$heatmap_facet == "Target_Type" & input$heatmap_x == "Target") {
        p <- p + facet_grid(reformulate(input$heatmap_facet,"."), scales = "free_x")   
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
        Error = mean(err),
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
          Error = mean(err),
          Skill = mean(exp(avg_score))) %>% 
        na.omit()
    }
     
    loc_y <- ifelse(input$location_y == "location_skill", 'Skill', 'Error')
     if (input$location_color == "None") {
       p <- ggplot(dat, aes_string(x = 'Epiweek', y = loc_y, group = 1)) + 
         geom_line(size = 1.1, alpha = 0.9)
     } else {
       p <- ggplot(dat, aes_string(x = 'Epiweek', y = loc_y)) + 
         geom_line(size = 1.1, alpha = 0.9)
     }

     if (input$location_color != "None") {
       p = p + aes_string(col = input$location_color, group = input$location_color)
     }
     
      if (input$location_facet != "None") {
        p = p + facet_wrap(as.formula(paste("~", input$location_facet)))#, drop = T, scales = "free_x", ncol = 3)
        if (!(input$location_facet %in% c("Target_Type", "Model_Type"))){
          ggplotly(p + labs(x = "Epiweek", y = loc_y) + scale_x_discrete(breaks = c(seq(43, 52, by = 2), seq(1, 18, by = 2))), tooltip=c("x","y","colour")) %>% 
            layout(height = 800,  margin = list(l = 80, b = 90))
        } else {
          ggplotly(p + labs(x = "Epiweek", y = loc_y) + scale_x_discrete(breaks = c(seq(43, 52, by = 2), seq(1, 18, by = 2))), tooltip=c("x","y","colour")) %>% 
            layout(height = 550,  margin = list(l = 80, b = 90))
        }
      } else {
        ggplotly(p + labs(x = "Epiweek", y = loc_y), tooltip=c("x","y","colour")) %>% 
             layout(height = 550)
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
        Skill = exp(avg_score),
        Error = mean(err)) %>% 
        filter(Season == input$season) %>% 
      na.omit()
    } else {
      dat <- all_season %>%
        group_by_("Epiweek",
                  ifelse(input$season_color != "None", input$season_color, "Epiweek"),
                  ifelse(input$season_facet != "None", input$season_facet, "Epiweek")) %>% 
        dplyr::summarize(
          Error = mean(err),
          Skill = mean(exp(avg_score))) %>% 
        na.omit()
    }
    
    seas_y <- ifelse(input$season_y == "season_skill", 'Skill', 'Error')
    if (input$season_color == "None") {
      p <- ggplot(dat, aes_string(x = 'Epiweek', y = seas_y, group = 1)) + 
        geom_line(size = 1.1, alpha = 0.9)
    } else {
      p <- ggplot(dat, aes_string(x = 'Epiweek', y = seas_y)) + 
        geom_line(size = 1.1, alpha = 0.9)
    }
    
    if (input$season_color != "None") {
      p = p + aes_string(col = input$season_color, group = input$season_color)
    }
    
    if (input$season_facet != "None") {
      p = p + facet_wrap(as.formula(paste("~", input$season_facet)))#, scales="free_x")
      if (!(input$season_facet %in% c("Target_Type", "Model_Type"))){
        ggplotly(p + labs(x = "Epiweek", y = seas_y) + scale_x_discrete(breaks = c(seq(43, 52, by = 2), seq(1, 18, by = 2))), tooltip=c("x","y","colour")) %>% 
          layout(height = 800, autosize=TRUE, margin = list(l = 80, b = 90))
      } else {
        ggplotly(p + labs(x = "Epiweek", y = seas_y) + scale_x_discrete(breaks = c(seq(43, 52, by = 2), seq(1, 18, by = 2))), tooltip=c("x","y","colour")) %>% 
          layout(height = 600, autosize = TRUE, margin = list(l = 80, b = 90))
      }
    } else {
      ggplotly(p + labs(x = "Epiweek", y = seas_y), tooltip=c("x","y","colour")) %>% 
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
        Skill = exp(avg_score),
        Error = mean(err)) %>% 
        filter(Model == input$model) %>% 
      na.omit()
  } else {
    dat <- all_model %>%
      group_by_("Epiweek",
                ifelse(input$model_color != "None", input$model_color, "Epiweek"),
                ifelse(input$model_facet != "None", input$model_facet, "Epiweek")) %>% 
      dplyr::summarize(
        Error = mean(err),
        Skill = mean(exp(avg_score))) %>% 
      na.omit()
  }
    mod_y <- ifelse(input$model_y == "model_skill", 'Skill', 'Error')
    if (input$model_color == "None") {
      p <- ggplot(dat, aes_string(x = 'Epiweek', y = mod_y, group = 1)) + 
        geom_line(size = 1.05, alpha = 0.9)
    } else {
      p <- ggplot(dat, aes_string(x = 'Epiweek', y = mod_y)) + 
        geom_line(size = 1.05, alpha = 0.9)
    }
    
    if (input$model_color != "None") {
      p = p + aes_string(col = input$model_color, group = input$model_color)
    }
    
    if (input$model_facet != "None") {
      p = p + facet_wrap(as.formula(paste("~", input$model_facet)))#, scales = "free_x")
      if (!(input$model_facet %in% c("Target_Type", "Model_Type"))){
        ggplotly(p + labs(x = "Epiweek", y = mod_y) + scale_x_discrete(breaks = c(seq(43, 52, by = 2), seq(1, 18, by = 2))), tooltip=c("x","y","colour")) %>% 
          layout(height = 800, autosize=TRUE, margin = list(l = 80, b = 90))
      } else {
        ggplotly(p + labs(x = "Epiweek", y = mod_y) + scale_x_discrete(breaks = c(seq(43, 52, by = 2), seq(1, 18, by = 2))), tooltip=c("x","y","colour")) %>% 
          layout(height = 550, autosize = TRUE, margin = list(l = 80, b = 90))
      }
    } else {
      ggplotly(p + labs(x = "Epiweek", y = mod_y), tooltip=c("x","y","colour")) %>% 
        layout(height = 550, autosize = TRUE)
    }
  })
  
})
