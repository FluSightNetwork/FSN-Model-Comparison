#script to load data and create relevant subserts for FSN model comparison app
#Evan Moore
#March 2018

#figure out how to remove this line
setwd("~/reichlab/FSN_Model_Comparison")

scores <- read_csv("data/scores.csv")
models <- read_csv("data/model-id-map.csv")
complete_models <- c(models$`model-id`[models$complete=="true"], "UTAustin-edm")
compartment <- c("CU-EAKFC_SEIRS", "CU-EAKFC_SIRS", "CU-EKF-SEIRS","CU-EKF_SIRS",
                 "CU-RHF_SIRS","CU-RHF_SEIRS","LANL-DBM")

## define column with scores of interest
SCORE_COL <- quo(`Multi bin score`)

all_target_bounds <- read_csv("data/all-target-bounds.csv")

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
scores_adj <- scores_adj %>% filter(!(Epiweek %in% c(41, 42, 53)))
scores_adj$Epiweek <- factor(scores_adj$Epiweek, levels = c(43:52, 1:18))

regions  <- unique(scores_adj$Location)
models <- complete_models
seasons <- unique(scores_adj$Season)
vars_col <- c("None", "Location", "Season", "Target", "Target_Type", "Model", "Model_Type")
vars_fac <- c("None", "Location", "Season", "Target", "Target_Type", "Model_Type")
