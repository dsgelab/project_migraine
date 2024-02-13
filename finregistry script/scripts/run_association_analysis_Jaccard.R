# Install and load the necessary packages if not already installed
suppressPackageStartupMessages({
  
  library(data.table)
  library(dplyr)
  library(lubridate)
  library(tibble)
})

source('/data/projects/project_mferro/project_migraine/file_paths.R')

association_analysis_logistic <- function(dataset, response_variable, PREVALENCE = TRUE) {
  # Create a matrix for results
  results <- matrix(NA, nrow = length(colnames(dataset)), ncol = 3, dimnames = list(colnames(dataset), c("Beta", "P-value", "se")))
  
  # Loop through the columns of the dataset
  for (column in colnames(dataset)) {
    if (column != response_variable & (column != "FINREGISTRYID") & column != "SEX" & column != "EVENT_AGE" & column != "year_purch") {
      if (PREVALENCE) {
        # Check if there is at least 1% cases in 2x2 matrix
        contingency_table <- table(dataset[[response_variable]], dataset[[column]])
        percentage_contingency_table <- prop.table(contingency_table, 1) * 100
        if (!is.na(percentage_contingency_table[4]) & percentage_contingency_table[4] >= 1) {
          # Perform the regression only if |correlation| between response and endpoint is >0.01
          correlation <- cor(as.numeric(dataset[[response_variable]]), as.numeric(dataset[[column]]))
          if (abs(correlation) >= 0.01 & !is.na(correlation) & correlation != "NA") {
            # Fit a logistic regression model
            model <- glm(as.formula(paste(response_variable, "~ SEX + year_birth + year_purch +", column)), data = dataset, family = "binomial")
            
            # Get Beta, p-value, se
            beta <- coef(model)
            p_value <- summary(model)$coefficients[, "Pr(>|z|)"]
            se <- summary(model)$coefficients[, "Std. Error"][5]
            
            # Save the results in the matrix
            results[column, "Beta"] <- beta[5] 
            results[column, "se"] <- se
            results[column, "P-value"] <- p_value[5] 
          }
        }
      } else {
        # If PREVALENCE is FALSE, perform regression without prevalence and correlation checks
        model <- glm(as.formula(paste(response_variable, "~ SEX + year_birth + year_purch +", column)), data = dataset, family = "binomial")
        
        # Get Beta, p-value, se
        beta <- coef(model)
        p_value <- summary(model)$coefficients[, "Pr(>|z|)"]
        se <- summary(model)$coefficients[, "Std. Error"][5]
        
        # Save the results in the matrix
        results[column, "Beta"] <- beta[5] 
        results[column, "se"] <- se
        results[column, "P-value"] <- p_value[5] 
      }
    }
  }
  
  # Remove rows with NA values in 'Beta', 'P_Value'
  results <- as.data.frame(results)
  results <- results[complete.cases(results$Beta, results$P_Value), ]
  
  # Return the matrix of results
  return(results)
}

save_results <- function(results, filename){
  output_path= "/data/projects/project_mferro/project_migraine/output/"
  output_filename = paste0(output_path,filename)
  write.csv(results, output_filename, row.names = FALSE)
}

# fetch cohort information

# fetch cohort information
cohort <- fread(study_population_file) %>% 
  arrange(FINREGISTRYID, EVENT_AGE) %>% 
  group_by(FINREGISTRYID) %>% 
  slice(1) %>% 
  mutate(
    year_purch=as.numeric(format(as.Date(DATE_FIRST_PURCH, format="%Y-%m-%d"),"%Y")),
    year_birth=as.numeric(format(as.Date(BIRTH_DATE, format="%Y-%m-%d"),"%Y"))
  ) %>%  
  select(FINREGISTRYID, F1,F2,F3,control_e, DATE_FIRST_PURCH, EVENT_AGE, SEX, year_birth,year_purch) 

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
      left_join(cohort, by = "FINREGISTRYID") %>%
      select(-DATE_FIRST_PURCH)
  }
  
  # Get estimates for F1, F2, F3, and controls
  #F1
  ep_F1 <- DB %>% filter(F1==1|control_e==1) %>% select(-F2,-F3,-control_e)
  response_variable <- "F1"
  if (name!="SES") {  results_association_F1 <- association_analysis_logistic(ep_F1, response_variable) }
  else {  results_association_F1 <- association_analysis_logistic(ep_F1, response_variable,PREVALENCE=FALSE)  }
  
  if(i==1){  F1_association <- rbind(my_matrix, results_association_F1) }
  else {  F1_association <- rbind(F1_association, results_association_F1)  }
  
  filename = paste0("association_",name,response_variable,".csv")
  save_results(results_association_F1,filename)
  
  #F2
  my_matrix <- data.frame()
  ep_F2 <- DB %>% filter(F2==1|control_e==1) %>% select(-F1,-F3,-control_e)
  response_variable <- "F2"
  if (name!="SES") {  results_association_F2 <- association_analysis_logistic(ep_F2, response_variable) }
  else {results_association_F2 <- association_analysis_logistic(ep_F2, response_variable,PREVALENCE=FALSE)  }
  
  if(i==1){  F2_association <- rbind(my_matrix, results_association_F2) }
  else {  F2_association <- rbind(F2_association, results_association_F2)  }
  filename = paste0("association_",name,response_variable,".csv")
  save_results(results_association_F2,filename)
  
  #F3
  ep_F3 <- DB %>% filter(F3==1|control_e==1) %>% select(-F1,-F2,-control_e)
  response_variable <- "F3"
  if (name!="SES") {  results_association_F3 <- association_analysis_logistic(ep_F3, response_variable) }
  else {results_association_F3 <- association_analysis_logistic(ep_F3, response_variable,PREVALENCE=FALSE)  }
  
  if(i==1){  F3_association <- rbind(my_matrix, results_association_F3) }
  else {  F3_association <- rbind(F3_association, results_association_F3)  }
  filename = paste0("association_",name,response_variable,".csv")
  save_results(results_association_F3,filename)
  
  #CTRL
  ep_CTRL <- DB %>% select(-F1,-F2,-F3)
  response_variable <- "control_e"
  if (name!="SES") {  results_association_CTRL <- association_analysis_logistic(ep_CTRL, response_variable) }
  else {results_association_CTRL <- association_analysis_logistic(ep_CTRL, response_variable,PREVALENCE=FALSE)  }
  
  if(i==1){  CTRL_association <- rbind(my_matrix, results_association_CTRL) }
  else {  CTRL_association <- rbind(CTRL_association, results_association_CTRL)  }
  filename = paste0("association_",name,response_variable,".csv")
  save_results(results_association_CTRL,filename)
}

########## CHECK JACCARD DISTANCE BETWEEN ENDPOINTS, DROP IF J>0.9 ################
F1_association <- F1_association %>% rownames_to_column("ENDPOINT") 
F2_association <- F2_association %>% rownames_to_column("ENDPOINT") 
F3_association <- F3_association %>% rownames_to_column("ENDPOINT") 

list_F1 <-  unique(F1_association$ENDPOINT) 
list_F2 <-  unique(F2_association$ENDPOINT) 
list_F3 <-  unique(F3_association$ENDPOINT) 

# keep only endpoints that passed the prevalence check
dataset  = fread(endpoint_file) 
dataset <- dataset %>% select(any_of(c(list_F1,list_F2,list_F3)))

threshold_simil <- 0.9

n_cols <- ncol(dataset)
matrix_Jaccard <- matrix(1, ncol = n_cols, nrow = n_cols,dimnames = list(colnames(dataset), colnames(dataset)))

for (i in 1:(n_cols - 1)) {
  for (j in (i + 1):n_cols) {
    current_col <- colnames(dataset[, ..i])
    next_col <- colnames(dataset[, ..j])
    
    cont_table <- table(dataset[[current_col]], dataset[[next_col]])
    #Jaccard distance = (M.11 / (M.11 + M.10 + M.01))
    jaccard_distance <- (cont_table[4] / (cont_table[4] + cont_table[2] + cont_table[3]))
    
    matrix_Jaccard[i, j] <- jaccard_distance

  }
}

heatmap(matrix_Jaccard, Rowv = NA, Colv = NA, col = c("white", "red"),scale = "none") 
        

high_simil_index <- which(matrix_Jaccard > threshold_simil, arr.ind = TRUE)

col_names <- colnames(matrix_Jaccard)[high_simil_index[, 2]]
row_names <- rownames(matrix_Jaccard)[high_simil_index[, 1]]

coord_high_sim <- data.frame(ColName = col_names, RowName = row_names)
coord_high_sim <- coord_high_sim %>%filter(ColName!=RowName)
print(coord_high_sim)

#modify this list according to your list
endpoint_to_remove <- c(
    "F5_DEPRESSIO", 
    ...
)

F1_association <- F1_association %>% filter(!ENDPOINT %in% endpoint_to_remove) %>% mutate(FDR_p_values = p.adjust(`P-value`, method = "BH")) %>% arrange(FDR_p_values) %>%  select(-"P-value") 
F2_association <- F2_association %>% filter(!ENDPOINT %in% endpoint_to_remove) %>%  mutate(FDR_p_values = p.adjust(`P-value`, method = "BH")) %>% arrange(FDR_p_values) %>%   select(-"P-value")
F3_association <- F3_association %>%  filter(!ENDPOINT %in% endpoint_to_remove) %>% mutate(FDR_p_values = p.adjust(`P-value`, method = "BH")) %>% arrange(FDR_p_values) %>%  select(-"P-value")

#  significative coefficients and se
Beta_F1_sign <- F1_association %>% filter(FDR_p_values < 0.05) %>% mutate (OR=exp(Beta), CI_low = exp(Beta - 1.96 * se), CI_high = exp(Beta + 1.96 * se))
Beta_F2_sign <- F2_association %>% filter(FDR_p_values < 0.05)%>% mutate (OR=exp(Beta),CI_low = exp(Beta - 1.96 * se), CI_high = exp(Beta + 1.96 * se))
Beta_F3_sign <- F3_association %>% filter(FDR_p_values < 0.05)%>% mutate (OR=exp(Beta),CI_low = exp(Beta - 1.96 * se), CI_high = exp(Beta + 1.96 * se))

### PLOT BETAS by Type of Switching ###
betas <- bind_rows(Beta_F1_sign,
                   Beta_F2_sign,
                   Beta_F3_sign,
                   .id="Model") %>%
  mutate( type_failure= as.factor(case_when(
                Model==1 ~ "F1",
                Model==2 ~ "F2",
                Model==3 ~ "F3",
                TRUE~"None" )))


ggplot(betas, aes(y = reorder(ENDPOINT, -Beta), x = Beta, color = type_failure)) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_errorbar(aes(xmin = Beta - se, xmax = Beta + se), position = position_dodge(width = 0.2), width = 0.2) +
  labs(x = "Beta (s.e.)",
       y = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0))


# Extract column names for 'endpoint', 'medications', and 'prev_medications'
endpoint_columns <- colnames(fread(endpoint_file, header=TRUE, nrows=0))
drugs_columns <- colnames(fread(medication_file, header=TRUE, nrows=0))
SES_columns <- colnames(fread(SES_file, header=TRUE, nrows=0))

betas$subgroup <- ifelse(betas$ENDPOINT %in% endpoint_columns, "Clinical Endpoint",
                         ifelse(betas$ENDPOINT %in% drugs_columns, "Drugs"))


### PLOT OVERALL by type of variable ###
ggplot(betas, aes(Beta, ENDPOINT, color = type_failure)) + 
  geom_point(size=4) +
  geom_errorbar(aes(xmin = Beta - se, xmax = Beta + se)) +
  labs(x="Beta (s.e.)", y="") +
  facet_grid(subgroup ~ ., scales = "free", space = "free") +
  theme(strip.text.y = element_text(angle=0)) +
  theme_bw()
 

library(ggrepel)
top_n <- 5
# Top n betas by failure group
top_endpoints_by_group <- betas %>%
  arrange(type_failure, desc(abs(Beta))) %>%
  group_by(type_failure) %>%
  slice_head(n = top_n) %>% pull(ENDPOINT)

ggplot(betas, aes(x = Beta, y = reorder(subgroup, Beta), color = type_failure)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2), size = 3) +
  labs(x = "Beta", y = "Category") +
  theme_be()  +
  geom_label_repel(data = betas[betas$ENDPOINT %in% top_endpoints_by_group, ],
                   aes(x = Beta, y = reorder(subgroup, Beta), label = ENDPOINT),
                   box.padding = 0.5, force = 2, segment.color = 'grey50', segment.size = 0.5, size = 3) +
  facet_grid(~type_failure, scales = "free_y") + 
  theme(legend.position = "none")


# PLOT BY TYPE FAILURE (eg F3)
betas_F3 <- betas %>% filter(type_failure=="F3")
ggplot(betas_F3, aes(y = reorder(ENDPOINT, -Beta), x = Beta )) +
  geom_point(position = position_dodge(width = 0.2), size = 3) +
  geom_errorbar(aes(xmin = Beta - se, xmax = Beta + se), position = position_dodge(width = 0.2), width = 0.2) +
  labs(title="F3", x = "Beta (SE)", y = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0))

