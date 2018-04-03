#script to load data and create relevant subsets for FSN model comparison app
#Evan Moore
#March 2018

scores <- read_csv("data/scores.csv")
models <- read_csv("data/model-id-map.csv")
complete_models <- c(models$`model-id`[models$complete=="true"], "UTAustin-edm")
compartment <- c("CU-EAKFC_SEIRS", "CU-EAKFC_SIRS", "CU-EKF_SEIRS","CU-EKF_SIRS",
                 "CU-RHF_SIRS","CU-RHF_SEIRS","LANL-DBM")
backfill <- c("LANL-DBM")

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
scores_adj <- scores_adj %>% filter(!(Epiweek %in% c(41, 42, 53)))
scores_adj$Epiweek <- factor(scores_adj$Epiweek, levels = c(43:52, 1:18))

regions  <- c("All Regions", unique(scores_adj$Location))
models <- c("All Models", complete_models)
seasons <- c("All Seasons", unique(scores_adj$Season))
vars_col <- c("None", "Location", "Season", "Target", "Target_Type", "Model", "Model_Type")
vars_fac <- c("None", "Location", "Season", "Target", "Target_Type", "Model_Type")

heatmap_x <- c("Location", "Season", "Target")
heatmap_fac <- c("None", "Target_Type")
heatmap_highlight <- c("None", "Compartmental","Backfill")

all_location <- scores_adj %>% 
  group_by(Epiweek, Season, Target, Target_Type, Model, Model_Type) %>% 
  summarise(avg_score = mean(score_adj),Skill = exp(avg_score))

all_season <- scores_adj %>% 
  group_by(Epiweek, Location, Target, Target_Type, Model, Model_Type) %>% 
  summarise(avg_score = mean(score_adj),Skill = exp(avg_score))

all_model <- scores_adj %>% 
  group_by(Epiweek, Season, Target, Target_Type, Location, Model_Type) %>% 
  summarise(avg_score = mean(score_adj),Skill = exp(avg_score))

#taken from https://stackoverflow.com/questions/39694490/highlighting-individual-axis-labels-in-bold-using-ggplot2
colorado <- function(src, boulder) {
  if (!is.factor(src)) src <- factor(src)                   # make sure it's a factor
  src_levels <- levels(src)                                 # retrieve the levels in their order
  brave <- boulder %in% src_levels                          # make sure everything we want to make bold is actually in the factor levels
  if (all(brave)) {                                         # if so
    b_pos <- purrr::map_int(boulder, ~which(.==src_levels)) # then find out where they are
    b_vec <- rep("plain", length(src_levels))               # make'm all plain first
    b_vec[b_pos] <- "bold"                                  # make our targets bold
    b_vec                                                   # return the new vector
  } else {
    stop("All elements of 'boulder' must be in src")
  }
}
