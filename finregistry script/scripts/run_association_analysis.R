
# Install and load the necessary packages if not already installed
suppressPackageStartupMessages({
  
  library(data.table)
  library(dplyr)
  library(lubridate)
  
})

source('/data/projects/project_mferro/project_migraine/file_paths.R')
PREVALENCE_CUTOFF = 0.05

# define function for evaluationg all the associations
association_analysis_logistic <- function(dataset, response_variable) {

  # Create a matrix for results
  results <- matrix(NA, nrow=length(dataset), ncol = 2, 
                    dimnames = list(colnames(dataset), c( "Beta", "P-value")))
  
  # Loop through the columns of the dataset
  for (column in colnames(dataset)) {

    if (column != response_variable & (column != "FINREGISTRYID") & column != "SEX" & column != "EVENT_AGE" & column != "year_purch"){ 
      #Check if there is at least 1% cases in 2x2 matrix
      contingency_table <- table(dataset[[response_variable]], dataset[[column]])
      percentage_contingency_table <- prop.table(contingency_table,1) * 100
      
      if (!is.na(percentage_contingency_table[4]) & percentage_contingency_table[4]>=1){ 
        #Perform the regression only if |correlation| between response and endpoint is >0.01
        correlation <- cor(as.numeric(dataset[[response_variable]]), as.numeric(dataset[[column]])) 
        
        if (abs(correlation) >= PREVALENCE_CUTOFF &  !is.na(correlation) & correlation!="NA"){
          #Fit a logistic regression model
          model <- lm(as.formula(paste(response_variable, "~ SEX + EVENT_AGE +", column)), data = dataset) 
    
          #Get Beta and p-value
          beta <- coef(model)
          p_value <- summary(model)$coefficients[, "Pr(>|t|)"]
    
          # Save the results in the matrix
          results[column, "Beta"] <- beta[4] 
          results[column, "P-value"] <- p_value[4] 
        }
      }
    }
  }  

  # Remove rows with NA values in both 'Beta' and 'P_Value'
  results <- as.data.frame(results)
  results <- results[complete.cases(results$Beta, results$P_Value), ]
  
  # Return the matrix of results
  return(results)
}

save_results <- function(results, filename){
  output_path = "/data/projects/project_mferro/project_migraine/output/"
  output_filename = paste0(output_path,filename)
  write.csv(results, output_filename, row.names = FALSE)
}

#---------------------#
#####   ANALYSIS   ####
#---------------------#


# fetch cohort information
cohort <- fread(study_population_file) %>% 
  group_by(FINREGISTRYID) %>% 
  slice(1) %>% select(FINREGISTRYID, F1,F2,F3,control_e, DATE_FIRST_PURCH, EVENT_AGE, SEX)


# fetch extra information and evaluate associations
file_list = c(endpoint_file, medication_file)

for(file in file_list){

  ep_trpt_fail <- fread(endpoint_file, sep = ',') %>%
    left_join(cohort, by = "FINREGISTRYID")  %>%
    mutate(year_purch=as.numeric(format(as.Date(DATE_FIRST_PURCH, format="%Y-%m-%d"),"%Y"))) %>% 
    select(-date_of_birth, -start_of_followup, -end_of_followup, -DATE_FIRST_PURCH)

  # Get estimates for F1, F2, F3, and controls
  ep_F1 <- ep_trpt_fail %>% select(-F2,-F3,-control_e)
  response_variable <- "F1"
  results_association_F1 <- association_analysis_logistic(ep_F1, response_variable) %>%  mutate(FDR_p_values = p.adjust(`P-value`, method = "BH"))
  results_association_F1 = results_association_F1[order(results_association_F1$`P-value`),]
  filename = paste0("association_",file,response_variable,".csv")
  save_results(results_association_F1,filename)

  ep_F2 <- ep_trpt_fail %>% select(-F1,-F3,-control_e)
  response_variable <- "F2"
  results_association_F2 <- association_analysis_logistic(ep_F2, response_variable) %>%  mutate(FDR_p_values = p.adjust(`P-value`, method = "BH")) %>% arrange(`P-value`)
  results_association_F2 = results_association_F2[order(results_association_F2$`P-value`),]
  filename = paste0("association_",file,response_variable,".csv")
  save_results(results_association_F2,filename)

  ep_F3 <- ep_trpt_fail %>% select(-F1,-F2,-control_e)
  response_variable <- "F3"
  results_association_F3 <- association_analysis_logistic(ep_F3, response_variable)  %>%  mutate(FDR_p_values = p.adjust(`P-value`, method = "BH"))
  results_association_F3 = results_association_F3[order(results_association_F3$`P-value`),]
  filename = paste0("association_",file,response_variable,".csv")
  save_results(results_association_F3,filename)

  ep_CTRL <- ep_trpt_fail %>% select(-F1,-F2,-F3)
  response_variable <- "control_e"
  results_association_CTRL <- association_analysis_logistic(ep_CTRL, response_variable) %>%  mutate(FDR_p_values = p.adjust(`P-value`, method = "BH"))
  results_association_CTRL = results_association_CTRL[order(results_association_CTRL$`P-value`),]
  filename = paste0("association_",file,response_variable,".csv")
  save_results(results_association_CTRL,filename)

}
