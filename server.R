library(shiny)

theme_set(theme_bw())

shinyServer(function(input, output, session) {
  
  ## side panel 2 
  output$heatmapPlot <- renderPlot({
    
    ## process data for plot  
    dat <- scores_adj %>%
      ## conditionally `group_by` using string inputs
      ## grouping by same variable twice has no effect - used when no facet is selected
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
    
    ## used to specify precision of heatmap labels
    specify_decimal <- function(x, k=0) trimws(format(round(x, k), nsmall=k))
    
    ## heatmap plot
    p <- ggplot(dat, aes_string(x=input$heatmap_x, y="Model", fill="pct_diff_baseline_skill")) +
      geom_tile() + ylab(NULL) + xlab(NULL) +
      geom_text(aes(label=specify_decimal(Skill, 2))) +
      scale_fill_gradient2(name = "% change \nfrom baseline") +
      theme_minimal() + #override theme_bw()
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ## conditionally highlight based on input
    if (input$heatmap_highlight != "None"){
      if (input$heatmap_highlight == "Compartmental"){
        p <- p + theme(axis.text.y=element_text(face=highlight(dat$Model, compartment, "bold"), color = highlight(dat$Model, compartment, "col")))
      } else if (input$heatmap_highlight == "Backfill") {
        p <- p + theme(axis.text.y=element_text(face=highlight(dat$Model, backfill, "bold"), color = highlight(dat$Model, backfill, "col")))
      } else {
        p <- p + theme(axis.text.y=element_text(face=highlight(dat$Model, ensemble, "bold"), color = highlight(dat$Model, ensemble, "col")))
      }
    ## if no highlight, make all font black
    } else {
      p <- p + theme(axis.text.y = element_text(color = "black"))
    }
    
    ## conditionally add facet if selected
    if (input$heatmap_facet != "None"){
      ## special case for when target and target type both selected
      if (input$heatmap_facet == "Target_Type" & input$heatmap_x == "Target") {
        p <- p + facet_grid(reformulate(input$heatmap_facet,"."), scales = "free_x")   
      ## regular case for otherwise  
      } else {
        p <- p + facet_grid(reformulate(".",input$heatmap_facet)) 
      }
    }
    p
  ## size specified after renderPlot() function for non-interactive ggplot output
  }, height = 600, width = 600)

  ## side panel 3 - all additional panels have the same structure, so the comments
  ## apply to the code used in side panels 4 and 5 in the same way they do here.
  output$locationPlot <- renderPlotly({
    
    ## normal case - conditionally group by user inputs and filter based on given location 
    if (input$location != "All Regions"){
     dat <- scores_adj %>%
     group_by_("Location", "Epiweek",
        ifelse(input$location_color != "None", input$location_color, "Location"),
        ifelse(input$location_facet != "None", input$location_facet, "Epiweek")) %>% 
     dplyr::summarize(
        Error = mean(err),
        avg_score = mean(score_adj),
        Skill = exp(avg_score)) %>% 
      dplyr::filter(Location == input$location) %>% 
       na.omit()
    ## special case for when all regions option is selected 
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
    
    ## specify y-axis label based on radio button input 
    loc_y <- ifelse(input$location_y == "location_skill", 'Skill', 'Error')
    
    ## create ggplot object, with group = 1 if no color option selected
    if (input$location_color == "None") {
      p <- ggplot(dat, aes_string(x = 'Epiweek', y = loc_y, group = 1)) + 
      geom_line(size = 1.1, alpha = 0.9)
    } else {
      p <- ggplot(dat, aes_string(x = 'Epiweek', y = loc_y)) + 
      geom_line(size = 1.1, alpha = 0.9)
    }

    ## add color if selected   
    if (input$location_color != "None") {
      p = p + aes_string(col = input$location_color, group = input$location_color)
    }
     
    ## add facet is selected
    if (input$location_facet != "None") {
      p = p + facet_wrap(as.formula(paste("~", input$location_facet)))#, scales = "free_x") - removes unused plots in facet wrap, but changes sizes of rows
      ## add facet, increasing height if selected facet has many different levels
      if (!(input$location_facet %in% c("Target_Type", "Model_Type"))){
        ggplotly(p + labs(x = "Epiweek", y = loc_y) + scale_x_discrete(breaks = c(seq(43, 52, by = 2), seq(1, 18, by = 2))), tooltip=c("x","y","colour")) %>% 
        layout(height = 800,  margin = list(l = 80, b = 90)) #adjust x and y margin so axis labels are not cut off
      ## else if target or model type selected, height is normal 
      } else {
        ggplotly(p + labs(x = "Epiweek", y = loc_y) + scale_x_discrete(breaks = c(seq(43, 52, by = 2), seq(1, 18, by = 2))), tooltip=c("x","y","colour")) %>% 
        layout(height = 550,  margin = list(l = 80, b = 90))
      }
    ## otherwise, output plot with regular height   
    } else {
      ggplotly(p + labs(x = "Epiweek", y = loc_y), tooltip=c("x","y","colour")) %>% 
      layout(height = 550)
    }
  })
  
  ## side panel 4 
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
        dplyr::filter(Season == input$season) %>% 
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
          layout(height = 550, autosize = TRUE, margin = list(l = 80, b = 90))
      }
    } else {
      ggplotly(p + labs(x = "Epiweek", y = seas_y), tooltip=c("x","y","colour")) %>% 
        layout(height = 550, autosize = TRUE)
    }
  })
  
  ## side panel 5 
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
        dplyr::filter(Model == input$model) %>% 
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
