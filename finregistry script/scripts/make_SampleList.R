
suppressPackageStartupMessages({
  
  library(data.table)
  library(dplyr)
  library(lubridate)
  
})

source('/data/projects/project_mferro/project_migraine/file_paths.R')

# import cohort to extract endpoints for
cohort <- fread(study_population_file)

# Make Sample File
WASHOUT_TIME = 30 # days
TIME_WINDOW = 10 # years

sample_file <- cohort %>% 
    arrange(FINREGISTRYID, EVENT_DAY) %>% 
    group_by(FINREGISTRYID) %>% 
    mutate(date_of_birth= as.Date(BIRTH_DATE),
        # start 10 years before first purchase (+ washout)
        start_of_followup=as.Date(DATE_FIRST_PURCH)- WASHOUT_TIME - (365*TIME_WINDOW),
        # end before first triptan purchase
        # NB: triptan avarage pack size is 28 
        end_of_followup=as.Date(DATE_FIRST_PURCH)- WASHOUT_TIME
    ) %>% 
    ungroup()%>%
    select(FINREGISTRYID,date_of_birth, start_of_followup, end_of_followup) %>%
    distinct(FINREGISTRYID, .keep_all = TRUE)

outfile = paste0(SampleList,'_',as.character(TIME_WINDOW),'_years')
fwrite(sample_file, file=outfile, append = FALSE)













