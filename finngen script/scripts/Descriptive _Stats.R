source('/home/ivm/project_migraine/file_paths.R')

cohort <- fread(study_population_file) %>% arrange(FINNGENID, EVENT_AGE) %>% 
  mutate(year_purch=as.numeric(format(as.Date(DATE_FIRST_PURCH, format="%d/%m/%Y"),"%Y")),
         year_birth=as.numeric(format(as.Date(BIRTH_DATE, format="%Y-%m-%d"),"%Y"))) %>%  select(FINNGENID, F1,F2,F3,control_e, DATE_FIRST_PURCH, EVENT_AGE, SEX, year_birth,year_purch, ATC_CODE, APPROX_EVENT_DAY, EVENT_AGE, AGE_FIRST_PURCH) 

follow_up_duration <- cohort %>%
                      group_by(FINNGENID) %>%
                        summarize(
                          StartDate = min(APPROX_EVENT_DAY),
                          EndDate = max(APPROX_EVENT_DAY),
                          FollowUpDuration = as.numeric(difftime(max(APPROX_EVENT_DAY), min(APPROX_EVENT_DAY), units = "days")/365.25),
                          TOT_TRPT_PURCH=  n()
                          )


cohort <- cohort %>% left_join(follow_up_duration, by = "FINNGENID") %>% arrange(FINNGENID, EVENT_AGE) %>% group_by(FINNGENID) %>% slice(1) %>% 
  mutate(age_cat = cut(AGE_FIRST_PURCH, breaks = c(18, 30, 50, Inf), labels = c("18-29", "30-49", "50+"), right= FALSE))
  

my_db <- transform(cohort, switch_type = ifelse(F1 == 1, "F1",
                                                   ifelse(F2 == 1, "F2",
                                                          ifelse(F3 == 1, "F3",
                                                                 ifelse(control_e == 1, "CTRL", "NA"))))) %>% mutate(switch_type=as.factor(switch_type))


sum_stats_quant <- function(dataset, type_switch) {
  numeric_columns <- sapply(dataset, is.numeric)
  
  for (col_name in names(numeric_columns)[numeric_columns]) {
    variable_data <- dataset[[col_name]]
    ks_test <- suppressWarnings(ks.test(dataset[[col_name]], "pnorm"))
    
    if (ks_test$p.value >= 0.05) 
    { 
      ANOVA <- aov(variable_data ~ type_switch, data = dataset)
      p_value <- summary(ANOVA)[[1]]$`Pr(>F)`[1]
      
      mean <- round(mean(variable_data, na.rm = TRUE) ,2)
      sd <- round(sd(variable_data, na.rm = TRUE) ,2)
      
      cat("Variable:", col_name, "| p-value: ",p_value," | ","Mean (sd): ",mean," (",sd, ") ", "\n")
      
      for (level in unique(type_switch)) 
        {
          subset_data <- variable_data[type_switch == level]
          cat("| ", level, "\n")
          subset_data <- variable_data[type_switch == level]
          mean <- round(mean(subset_data, na.rm = TRUE) ,2)
          sd <- round(sd(subset_data, na.rm = TRUE) ,2)
          cat(" Mean (sd):", mean," (",sd, ") ", "\n")
          
        }
    }
    else
      {
        KRUSKAL <- kruskal.test(variable_data ~ type_switch, data = dataset)
        p_value <- KRUSKAL$p.value
        
        Median <- round(median(variable_data, na.rm = TRUE) ,2)
        Q1 <- round(quantile(variable_data, probs = 0.25, na.rm = TRUE) ,2)
        Q3 <- round(quantile(variable_data, probs = 0.75, na.rm = TRUE) ,2)
        cat("Variable:", col_name, "| p-value ",p_value," |","Median [IQR]:", Median," [", Q1,"-",Q3,"]", "\n")
        
        for (level in unique(type_switch)) 
          { 
            subset_data <- variable_data[type_switch == level]
            Median <- round(median(subset_data, na.rm = TRUE) ,2)
            Q1 <- round(quantile(subset_data, probs = 0.25, na.rm = TRUE) ,2)
            Q3 <- round(quantile(subset_data, probs = 0.75, na.rm = TRUE) ,2)
            cat(level, "| ", Median," [", Q1,"-",Q3,"]", "\n")
            
          }
      }
      
      cat("\n ########################################## \n")
    }
  }



data_list_quant <- c("EVENT_AGE","FollowUpDuration")
db_quant <- my_db %>% select(FINNGENID, switch_type, all_of(data_list_quant))
sum_stats_quant(db_quant, db_quant$switch_type)

#Categorical
endpoint_baseline  = fread("/home/ivm/project_migraine/data/ep_baseline")  #modify this path 
endpoint  = fread(endpoint_file) 
drugs= fread(medication_file) 
prev_med= fread(prev_med_file) 
SES_data= fread(SES_file) 

var_endpoint <- c( "T2D", "F5_PSYCH", "G6_NEURO", "G6_MIGRAINE", "I9_CVD", "F5_DEPRESSIO" )
var_drugs <- c("C02", "N02", "A02", "J02",  "N05", "M01", "N06")

data_list_char <- c("FINNGENID", "SEX","age_cat","ATC_CODE", var_endpoint, var_drugs )

db_char <- my_db %>%
  left_join(endpoint, by = "FINNGENID") %>% select(-date_of_birth, -start_of_followup, -end_of_followup, -DATE_FIRST_PURCH)  %>%  
  left_join(drugs, by = "FINNGENID") %>% select(-date_of_birth, -start_of_followup, -end_of_followup) %>%
  left_join(prev_med, by = "FINNGENID") %>% select(-date_of_birth, -start_of_followup, -end_of_followup) %>% 
  select( switch_type, any_of(data_list_char)) %>% mutate(SEX=as.factor(SEX))

for (col in colnames(db_char)) 
{
  if (col != "FINNGENID" & col != "switch_type") 
  { 
    p_value <- chisq.test(table(db_char[[col]],db_char$switch_type))$p.value
    cat(paste(col, "p_value: ",p_value, "\n"))
    
    cat(paste("Frequencies for", col, ":\n"))
    abs_freq <- table(db_char[[col]],db_char$switch_type)
    print(abs_freq)
    percent_freq <- prop.table(abs_freq, margin = 2)*100
    cat(paste("Col pct for", col, ":\n")) 
    print(percent_freq)
    cat("\n")
  }
}
