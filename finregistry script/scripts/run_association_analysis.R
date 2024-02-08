# Install and load the necessary packages if not already installed
suppressPackageStartupMessages({
  
  library(data.table)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(tibble)
  
})

source('/data/projects/project_mferro/project_migraine/file_paths.R')

#prevalence of ednpoint_x in group of interest (e.g. at least 1% of F1 had experienced the endpoint x)
PREVALENCE_CUTOFF=1; 

association_analysis_logistic <- function(dataset, response_variable, PREVALENCE=TRUE) {
  # Create a matrix for results
  results <- matrix(NA, nrow=length(dataset), ncol = 2,dimnames = list(colnames(dataset), c( "Beta", "P-value"))) #check chagne ep_F1 to dataset
  
  # Loop through the columns of the dataset
  for (column in colnames(dataset)) {
    
    if (column != response_variable & (column != "FINREGISTRYID") & column != "SEX" & column != "EVENT_AGE"  & column != "BIRTH_DATE" & column != "year_purch" & column !="year_birth") 
    { 
      if (PREVALENCE) 
      {
        #Check if there is at least PREVALENCE(%) cases in 2x2 matrix
        contingency_table <- table(dataset[[response_variable]], dataset[[column]])
        percentage_contingency_table <- prop.table(contingency_table,1) * 100
        if (!is.na(percentage_contingency_table[4]) & percentage_contingency_table[4]>=PREVALENCE_CUTOFF)
        { 
          #Perform the regression only if |correlation| between response andl endpoint is >0.01
          correlation <- cor(as.numeric(dataset[[response_variable]]), as.numeric(dataset[[column]])) 
          if (abs(correlation) >= 0.01 &  !is.na(correlation) & correlation!="NA")
          {
            #Fit a logistic regression model
            model <- glm(as.formula(paste(response_variable, "~ SEX + year_birth + year_purch + ", column)), data = dataset, family="binomial") 
            
            #Get Beta and p-value
            beta <- coef(model)
            p_value <- summary(model)$coefficients[, "Pr(>|z|)"]
            
            # Save the results in the matrix
            results[column, "Beta"] <- beta[5] 
            results[column, "P-value"] <- p_value[5] 
          }
        }
      }
      else 
      {
        # If PREVALENCE is FALSE, perform regression without prevalence and correlation checks
        model <- glm(as.formula(paste(response_variable, "~ SEX + year_birth + year_purch +", column)), data = dataset, family="binomial") 
        
        beta <- coef(model)
        p_value <- summary(model)$coefficients[, "Pr(>|z|)"]
        
        results[column, "Beta"] <- beta[5] 
        results[column, "P-value"] <- p_value[5] 
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
  output_path= "/data/projects/project_mferro/project_migraine/output/"
  output_filename = paste0(output_path,filename)
  write.csv(results, output_filename, row.names = TRUE)
}


#---------------------#
#####   ANALYSIS   ####
#---------------------#

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

#------------------------#
##### FDR calculation ####
#------------------------#

# FDR, arrange by p-values , select only statistically significative factors
F1_association <- F1_association %>%  mutate(FDR_p_values = p.adjust(`P-value`, method = "BH")) %>% arrange(FDR_p_values) %>% filter(FDR_p_values < 0.05) %>% rownames_to_column("ENDPOINT")
F2_association <- F2_association %>%  mutate(FDR_p_values = p.adjust(`P-value`, method = "BH")) %>% arrange(FDR_p_values) %>% filter(FDR_p_values < 0.05) %>% rownames_to_column("ENDPOINT")
F3_association <- F3_association %>%  mutate(FDR_p_values = p.adjust(`P-value`, method = "BH")) %>% arrange(FDR_p_values) %>% filter(FDR_p_values < 0.05) %>% rownames_to_column("ENDPOINT")
CTRL_association <- CTRL_association %>%  mutate(FDR_p_values = p.adjust(`P-value`, method = "BH")) %>% arrange(FDR_p_values) %>% filter(FDR_p_values < 0.05) %>% rownames_to_column("ENDPOINT")

#------------------------#
#####    Plotting     ####
#------------------------#

# PLOTS coefficients
Beta_F1 <- F1_association %>% rename(Beta_F1=Beta)
Beta_F2 <- F2_association %>% rename(Beta_F2=Beta)
Beta_F3 <- F3_association %>% rename(Beta_F3=Beta)
Beta_CTRL <- CTRL_association %>% rename(Beta_CTRL=Beta)

mydata <- Beta_F1 %>% 
  full_join(Beta_F2, by="ENDPOINT") %>% 
  full_join(Beta_F3, by="ENDPOINT") %>% 
  full_join(Beta_CTRL, by="ENDPOINT")%>% 
  select(ENDPOINT, Beta_F1, Beta_F2, Beta_F3, Beta_CTRL)

### F1 vs F2 ###
ggplot(mydata, aes(x=Beta_F1, y=Beta_F2,label=ENDPOINT))+ 
  geom_point() +geom_text(hjust=0, vjust=0) +
  geom_abline(linetype="dashed",intercept = 0, slope = 1) 

### F1 vs F3 ###
ggplot(mydata, aes(x=Beta_F1, y=Beta_F3,label=ENDPOINT))+ 
  geom_point() +geom_text(hjust=0, vjust=0) +
  geom_abline(linetype="dashed",intercept = 0, slope = 1) 

### F2 vs F3 ###
ggplot(mydata, aes(x=Beta_F2, y=Beta_F3,label=ENDPOINT))+ 
  geom_point() +geom_text(hjust=0, vjust=0) + 
  geom_abline(linetype="dashed",intercept = 0, slope = 1)


### PLOT BETAS by Type of Switching ###
betas <- bind_rows(F1_association,F2_association,F3_association,.id="Model") %>% 
  mutate(type_failure= as.factor(
    case_when(
      Model==1 ~ "F1",
      Model==2 ~ "F2",
      Model==3 ~ "F3",
      TRUE~"None")
    )) 

ggplot(betas, aes(x=reorder(ENDPOINT,-Beta), y=Beta, color=type_failure))+
        geom_point(position=position_dodge(width=0.4), size=3)+
        labs(x="Variable",y="Beta")+
        geom_hline(yintercept = 0)+
        theme_minimal()+
        theme(axis.text.x= element_text(angle=45,hjust=1))

rm(list=ls())
gc()


