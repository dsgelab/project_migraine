
suppressPackageStartupMessages({
  
  library(data.table)
  library(dplyr)
  library(lubridate)
  
})

source('/data/projects/project_mferro/project_migraine/file_paths.R')

# import cohort to extract endpoints for
cohort <- fread(study_population_file)

# Make Sample File
WASHOUT_TIME = 30 

sample_file <- cohort %>% 
    arrange(FINREGISTRYID, EVENT_DAY) %>% 
    group_by(FINREGISTRYID) %>% 
    mutate(date_of_birth= as.Date(BIRTH_DATE),
        # start from date of birth (first data available in registries)
        start_of_followup= as.Date(BIRTH_DATE), 
        # end before first triptan purchase
        # NB: triptan avarage pack size is 28 
        end_of_followup=as.Date(DATE_FIRST_PURCH)- WASHOUT_TIME
    ) %>% 
    ungroup()%>%
    select(FINREGISTRYID,date_of_birth, start_of_followup, end_of_followup) %>%
    distinct(FINREGISTRYID, .keep_all = TRUE)

fwrite(sample_file, file=SampleList, append = FALSE)













