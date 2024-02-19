###############################################################################
# 
# 1. prepare environment and functions
# 2. fetch data and extract+save result
# 3. evaluate Jaccard distance
# 4. plot final results
# 
###############################################################################

library(data.table)
library(dplyr)
library(lubridate)
library(tibble)
library(ggrepel)

source('/data/projects/project_mferro/project_migraine/file_paths.R')


###############
####   1   ####
###############

OUTPUT_PATH = "/data/projects/project_mferro/project_migraine/output/"
COVARIATES_TO_EXCLUDE = c("FINREGISTRYID","SEX","year_purch","year_birth")
PREVALENCE_CUTOFF = 0.01


association_analysis_logistic <- function(dataset, response_variable, PREVALENCE = TRUE) {
  
  # Create empty matrix for results
  results <- matrix(NA, nrow = length(colnames(dataset)), ncol = 3, dimnames = list(colnames(dataset), c("Beta", "p_value", "se")))
  
  # Loop through the columns of the dataset (representing all the covariates)
  for (covariate in colnames(dataset)) {
    if ( (covariate != response_variable) & !(covariate %in% COVARIATES_TO_EXCLUDE) ) {
      if (PREVALENCE) {
        # Check if there is at least a 1% prevalence of the covariate in the response cases
        contingency_table <- table(dataset[[response_variable]], dataset[[covariate]])
        percentage_contingency_table <- prop.table(contingency_table, 1) * 100
        
        if ((dim(percentage_contingency_table)==c(2,2)) & !is.na(percentage_contingency_table[4]) & (percentage_contingency_table[4] >= PREVALENCE_CUTOFF*100))
          {
          # Fit a logistic regression model
          model <- glm(as.formula(paste(response_variable, "~ SEX + year_birth + year_purch +", covariate)), data = dataset, family = "binomial")
          # Save the model results in the matrix
          results[covariate, "Beta"]    <- coef(model)[5] 
          results[covariate, "p_value"] <- summary(model)$coefficients[, "Pr(>|z|)"][5] 
          results[covariate, "se"]      <- summary(model)$coefficients[, "Std. Error"][5]
          }
      } 
      else {
        # perform regression without prevalence checks
        model <- glm(as.formula(paste(response_variable, "~ SEX + year_birth + year_purch +", covariate)), data = dataset, family = "binomial")
        # Save the model results in the matrix
        results[covariate, "Beta"]    <- coef(model)[5] 
        results[covariate, "p_value"] <- summary(model)$coefficients[, "Pr(>|z|)"][5] 
        results[covariate, "se"]      <- summary(model)$coefficients[, "Std. Error"][5]
      }
    }
  }
  
  # Remove rows with NA values in 'Beta', 'p_Value'
  results <- as.data.frame(results)
  results <- results[complete.cases(results$Beta, results$p_value), ]
  return(results)
}


save_results <- function(results, filename){
  output_path= OUTPUT_PATH
  output_filename = paste0(output_path,filename)
  write.csv(results, output_filename, row.names = FALSE)
}


###############
####   2   ####
###############

# fetch cohort information
cohort <- fread(study_population_file) %>% 
  arrange(FINREGISTRYID, EVENT_AGE) %>% 
  group_by(FINREGISTRYID) %>% 
  slice(1) %>% 
  mutate(
    year_purch=as.numeric(format(as.Date(DATE_FIRST_PURCH, format="%Y-%m-%d"),"%Y")),
    year_birth=as.numeric(format(as.Date(BIRTH_DATE, format="%Y-%m-%d"),"%Y"))
  ) %>%  
  select(FINREGISTRYID, SWITCH_1, SWITCH_2, SWITCH_3, NO_SWITCH, DATE_FIRST_PURCH, EVENT_AGE, SEX, year_birth, year_purch) 

# fetch extra information and evaluate associations
file_list = c(endpoint_file, medication_file, SES_file)
name_list = c('endpoint','medication','SES')
my_matrix <- data.frame()

for( i in seq(1,length(name_list)) ){
  
  file = file_list[i]
  name = name_list[i]
  
  
  if (name!="SES") {
    DB <- fread(file, sep = ',') %>%
      left_join(cohort, by = "FINREGISTRYID")  %>%
      select(-date_of_birth, -start_of_followup, -end_of_followup, -DATE_FIRST_PURCH)
  }
  else{
    DB <- fread(file, sep = ',') %>%
      select(-SEX) %>%
      rename( 
        psychiatric_residential_care='247_psychiatric_residential_care', 
        residential_care_housing_under_65yo='247_residential_care_housing_under_65yo'
      ) %>%
      # standardize continuous columns
      mutate(across(c('total_benefits','self_rated_health_moderate_or_poor_scaled_health_and_welfare_indicator'), scale)) %>%
      left_join(cohort, by = "FINREGISTRYID") %>%
      select(-DATE_FIRST_PURCH)
  }
  
  
  # Get estimates for each response variable
  DB_SWITCH_1 <- DB %>% filter(SWITCH_1==1|NO_SWITCH==1) %>% select(-SWITCH_2,-SWITCH_3,-NO_SWITCH)
  response_variable <- "SWITCH_1"
  if (name!="SES") { results_association <- association_analysis_logistic(DB_SWITCH_1, response_variable) }
  else { results_association <- association_analysis_logistic(DB_SWITCH_1, response_variable, PREVALENCE=FALSE) }
  if (i==1) { switch_1_association <- rbind(my_matrix, results_association) }
  else { switch_1_association <- rbind(switch_1_association, results_association)  }
  filename = paste0("association_",name,response_variable,".csv")
  save_results(switch_1_association,filename)
  
  DB_SWITCH_2 <- DB %>% filter(SWITCH_2==1|NO_SWITCH==1) %>% select(-SWITCH_1,-SWITCH_3,-NO_SWITCH)
  response_variable <- "SWITCH_2"
  if (name!="SES") { results_association <- association_analysis_logistic(DB_SWITCH_2, response_variable) }
  else { results_association <- association_analysis_logistic(DB_SWITCH_2, response_variable, PREVALENCE=FALSE) }
  if (i==1) { switch_2_association <- rbind(my_matrix, results_association) }
  else { switch_2_association <- rbind(switch_2_association, results_association)  }
  filename = paste0("association_",name,response_variable,".csv")
  save_results(switch_2_association,filename)
  
  DB_SWITCH_3 <- DB %>% filter(SWITCH_3==1|NO_SWITCH==1) %>% select(-SWITCH_1,-SWITCH_2,-NO_SWITCH)
  response_variable <- "SWITCH_3"
  if (name!="SES") { results_association <- association_analysis_logistic(DB_SWITCH_3, response_variable) }
  else { results_association <- association_analysis_logistic(DB_SWITCH_3, response_variable, PREVALENCE=FALSE) }
  if (i==1) { switch_3_association <- rbind(my_matrix, results_association) }
  else { switch_3_association <- rbind(switch_3_association, results_association)  }
  filename = paste0("association_",name,response_variable,".csv")
  save_results(switch_3_association,filename)
}

###############
####   3   ####
###############


#Jaccard distance
# J = (M.11 / (M.11 + M.10 + M.01))
# where M is 2x2 matrix 

# drop one of the covariates if the Jaccard distance (J) is above 0.9
JACCARD_CUTOFF = 0.9

switch_1_association <- switch_1_association %>% rownames_to_column("ENDPOINT") 
switch_2_association <- switch_2_association %>% rownames_to_column("ENDPOINT") 
switch_3_association <- switch_3_association %>% rownames_to_column("ENDPOINT") 

list_1 <-  unique(switch_1_association$ENDPOINT) 
list_2 <-  unique(switch_2_association$ENDPOINT) 
list_3 <-  unique(switch_3_association$ENDPOINT) 

# keep only endpoints that passed the prevalence check
# NB: no medication and SES

dataset  = fread(endpoint_file) 
dataset <- dataset %>%
  select(any_of(c(list_1,list_2,list_3))) %>%
  select(-c('date_of_birth','start_of_followup','end_of_followup'))

n_cols <- ncol(dataset)
matrix_Jaccard <- matrix(0, ncol = n_cols, nrow = n_cols, dimnames = list(colnames(dataset), colnames(dataset)))

for (i in 1:(n_cols-1)) {
  for (j in (i+1):n_cols) {
    current_col <- colnames(dataset[, ..i])
    next_col <- colnames(dataset[, ..j])
    cont_table <- table(dataset[[current_col]], dataset[[next_col]])
    J <- (cont_table[4] / (cont_table[4] + cont_table[2] + cont_table[3]))
    matrix_Jaccard[i, j] <- J
  }
}

high_simil_index <- which(matrix_Jaccard > JACCARD_CUTOFF, arr.ind = TRUE)
coord_high_sim <- data.frame(
  ColName = colnames(matrix_Jaccard)[high_simil_index[, 2]], 
  RowName = rownames(matrix_Jaccard)[high_simil_index[, 1]]) %>%
  filter(ColName!=RowName)
print(coord_high_sim)
endpoint_to_remove <- coord_high_sim$ColName

###############
####   4   ####
###############

# prepare results for plotting
F1_association <- switch_1_association %>% filter(!ENDPOINT %in% endpoint_to_remove) %>% mutate(FDR_p_values = p.adjust(p_value, method = "BH")) %>% arrange(FDR_p_values) %>%  select(-"p_value") 
F2_association <- switch_2_association %>% filter(!ENDPOINT %in% endpoint_to_remove) %>%  mutate(FDR_p_values = p.adjust(p_value, method = "BH")) %>% arrange(FDR_p_values) %>%   select(-"p_value")
F3_association <- switch_3_association %>%  filter(!ENDPOINT %in% endpoint_to_remove) %>% mutate(FDR_p_values = p.adjust(p_value, method = "BH")) %>% arrange(FDR_p_values) %>%  select(-"p_value")
# converting to OR for plotting
Beta_F1_sign <- F1_association %>% filter(FDR_p_values < 0.05) %>% mutate (OR=exp(Beta), CI_low = exp(Beta - 1.96 * se), CI_high = exp(Beta + 1.96 * se))
Beta_F2_sign <- F2_association %>% filter(FDR_p_values < 0.05) %>% mutate (OR=exp(Beta), CI_low = exp(Beta - 1.96 * se), CI_high = exp(Beta + 1.96 * se))
Beta_F3_sign <- F3_association %>% filter(FDR_p_values < 0.05) %>% mutate (OR=exp(Beta), CI_low = exp(Beta - 1.96 * se), CI_high = exp(Beta + 1.96 * se))

#### PLOT BETAS by Type of Switching ####
betas <- bind_rows(Beta_F1_sign, Beta_F2_sign, Beta_F3_sign, .id="Model") %>%
  mutate( type_failure= as.factor(case_when(
    Model==1 ~ "SWITCH_1",
    Model==2 ~ "SWITCH_2",
    Model==3 ~ "SWITCH_3",
    TRUE~"None" )))

ggplot(betas, aes(y = reorder(ENDPOINT, -OR), x = OR, color = type_failure)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_errorbar(aes(xmin = CI_low, xmax = CI_high), position = position_dodge(width = 0.2), width = 0.2) +
  labs(x = "OR (CI_95%)", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0))

