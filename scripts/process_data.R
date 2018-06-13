## script to process data and create final scores file for FSN model comparison app
## Evan Moore
## June 2018

## load necessary data
scores <- read_csv("data/scores.csv")
models <- read_csv("data/model-id-map.csv")
point_ests <- read_csv("data/point_ests_adj.csv")
complete_models <- c(models$`model-id`[models$complete=="true"], "UTAustin-edm")
compartment <- c("CU-EAKFC_SEIRS", "CU-EAKFC_SIRS", "CU-EKF_SEIRS","CU-EKF_SIRS",
                 "CU-RHF_SIRS","CU-RHF_SEIRS","LANL-DBM")

## define column with scores of interest
SCORE_COL <- quo(`Multi bin score`)

scores_adj <- scores %>%
  filter(Model %in% complete_models) %>%
  ## if NA, NaN or <-10, set score to -10
  mutate(score_adj = dplyr::if_else(is.nan(!!SCORE_COL) | is.na(!!SCORE_COL) , 
                                    -10, 
                                    !!SCORE_COL),
         Target_Type = dplyr::if_else(Target %in% c("Season onset", "Season peak week", "Season peak percentage"),
                                      "seasonal", "k-week-ahead"),
         Model_Type = ifelse(Model %in% compartment, "Compartmental", "Non Compartmental")) %>%
  mutate(score_adj = dplyr::if_else(score_adj < -10 , -10, score_adj)) 

## remove extra weeks and adjust ordering of location and week variables
scores_adj <- scores_adj %>% filter(!(Epiweek %in% c(41, 42, 53)))
scores_adj$Epiweek <- factor(scores_adj$Epiweek, levels = c(43:52, 1:18))
scores_adj$Location <- factor(scores_adj$Location)
levels(scores_adj$Location) <- unique(scores_adj$Location)

## process point ests in similar manner and rename model variable to facilitate a join
point_ests <- point_ests %>% select(Model = model_name, Year, Epiweek = Calendar.Week, Target, err, Location) %>% 
  filter(!(Epiweek %in% c(41, 42, 53))) %>% 
  na.omit()
point_ests$Model <- plyr::mapvalues(point_ests$Model, from = unique(point_ests$Model), to = c("CU-EAKFC_SEIRS", 
                                                                                              "CU-EAKFC_SIRS", "CU-EKF_SEIRS", "CU-EKF_SIRS", "CU-RHF_SEIRS", "CU-RHF_SIRS", "CU-BMA",
                                                                                              "Delphi-BasisRegression", "Delphi-DeltaDensity1", "Delphi-EmpiricalBayes2", "Delphi-EmpiricalBayes1", 
                                                                                              "Delphi-EmpiricalFuture", "Delphi-EmpiricalTraj","Delphi-DeltaDensity2","Delphi-Stat",
                                                                                              "Delphi-Uniform","LANL-DBM","ReichLab-KCDE","ReichLab-KDE","ReichLab-SARIMA1","ReichLab-SARIMA2","UTAustin-edm"))
point_ests$Epiweek <- factor(point_ests$Epiweek, levels = c(43:52, 1:18))
point_ests$Location <- factor(point_ests$Location)
levels(point_ests$Location) <- unique(point_ests$Location)

## merge dataset to use in both skill and error plots
scores_adj_final <- merge(scores_adj, point_ests)
