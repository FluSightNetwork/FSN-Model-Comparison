## script to load data and create relevant subsets for FSN model comparison app
## Evan Moore
## March 2018

## load and adjust levels of data
scores_adj <- read_csv("data/scores_adj_final.csv")

scores_adj$Epiweek <- factor(scores_adj$Epiweek, levels = c(43:52, 1:18))
levels <- c("US National", "HHS Region 1", "HHS Region 2", "HHS Region 3", "HHS Region 4", "HHS Region 5", "HHS Region 6", "HHS Region 7", "HHS Region 8", "HHS Region 9", "HHS Region 10")
scores_adj$Location <- factor(scores_adj$Location, levels = levels)

## for highlight option in overall results
compartment <- c("CU-EAKFC_SEIRS", "CU-EAKFC_SIRS", "CU-EKF_SEIRS","CU-EKF_SIRS",
                 "CU-RHF_SIRS","CU-RHF_SEIRS","LANL-DBM")
backfill <- c("LANL-DBM")
ensemble <- c("Delphi-Stat")

## variables used in model UI 
regions  <- c("All Regions", levels)
models <- c("All Models", unique(scores_adj$Model))
seasons <- c("All Seasons", unique(scores_adj$Season))
vars_col <- c("None", "Location", "Season", "Target", "Target_Type", "Model", "Model_Type")
vars_fac <- c("None", "Location", "Season", "Target", "Target_Type", "Model_Type")

heatmap_x <- c("Location", "Season", "Target")
heatmap_fac <- c("None", "Target_Type")
heatmap_highlight <- c("None", "Compartmental","Backfill", "Ensemble")

## create subsets of data that average across all targets of interest
all_location <- scores_adj %>% 
  group_by(Epiweek, Season, Target, Target_Type, Model, Model_Type) %>% 
  summarise(err = mean(err), avg_score = mean(score_adj),Skill = exp(avg_score))

all_season <- scores_adj %>% 
  group_by(Epiweek, Location, Target, Target_Type, Model, Model_Type) %>% 
  summarise(err = mean(err), avg_score = mean(score_adj),Skill = exp(avg_score))

all_model <- scores_adj %>% 
  group_by(Epiweek, Season, Target, Target_Type, Location, Model_Type) %>% 
  summarise(err = mean(err), avg_score = mean(score_adj),Skill = exp(avg_score))

## function to conditionally highlight text in overall results tab
## adapted from https://stackoverflow.com/questions/39694490/highlighting-individual-axis-labels-in-bold-using-ggplot2
highlight <- function(src, boulder, type) {
  if (!is.factor(src)) src <- factor(src)                   # make sure it's a factor
  src_levels <- levels(src)                                 # retrieve the levels in their order
  brave <- boulder %in% src_levels                          # make sure everything we want to make bold is actually in the factor levels
  if (all(brave)) {                                         # if so
    b_pos <- purrr::map_int(boulder, ~which(.==src_levels)) # then find out where they are
    b_vec <- rep(ifelse(type == "bold", "plain", "black"), length(src_levels))               # make'm all plain first
    b_vec[b_pos] <- ifelse(type == "bold", "bold", "red")   # make our targets bold
    b_vec                                                   # return the new vector
  } else {
    stop("All elements of 'boulder' must be in src")
  }
}

