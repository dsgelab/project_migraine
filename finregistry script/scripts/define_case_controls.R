
####################################################
#
# 1. fetch data 
# 2. define cases
# 3. define controls
# 4. save results
# 
####################################################

# PREPARE ENVIRONMENT:
#NB: using ePouta shared env available packages

suppressPackageStartupMessages({
  
  library(data.table)
  library(dplyr)
  library(lubridate)
  
})

source('/data/projects/project_mferro/project_migraine/file_paths.R')

###############
####   1   ####
###############

cohort <- fread(study_population_file)

# for later sanity-checks
SC <- nrow(cohort)
ORIGINAL_N <- length(unique(cohort$FINREGISTRYID))

###############
####   2   ####
###############

# define switch 
cohort <- cohort %>% 
  group_by(FINREGISTRYID) %>%
  arrange(FINREGISTRYID) %>%
  mutate(SWITCH = ifelse(ATC_CODE != lag(ATC_CODE),1,0)) %>%
  mutate(SWITCH = ifelse(is.na(SWITCH),0,SWITCH))

# QC:
# table( cohort %>% summarise(TOT_SWITCH=sum(SWITCH)) %>% pull(TOT_SWITCH) )


TRPT_FAIL_1 <- cohort %>% summarise(TOT_SWITCH=sum(SWITCH)) %>% filter(TOT_SWITCH==1) %>% pull(FINREGISTRYID)
TRPT_FAIL_2 <- cohort %>% summarise(TOT_SWITCH=sum(SWITCH)) %>% filter(TOT_SWITCH==2) %>% pull(FINREGISTRYID)
TRPT_FAIL_3 <- cohort %>% summarise(TOT_SWITCH=sum(SWITCH)) %>% filter(TOT_SWITCH>=3) %>% pull(FINREGISTRYID)

cat(
  'number of failures to 1 triptan: ', length(TRPT_FAIL_1), 
  'in percentage: ',100*length(TRPT_FAIL_1)/ORIGINAL_N,'\n',
  'number of failures to 2 triptan: ', length(TRPT_FAIL_2), 
  'in percentage: ',100*length(TRPT_FAIL_2)/ORIGINAL_N,'\n',
  'number of failures to >= 3 triptan: ', length(TRPT_FAIL_3), 
  'in percentage: ',100*length(TRPT_FAIL_3)/ORIGINAL_N,'\n'
)
  
###############
####   3   ####
###############
  
CONTROL <- cohort %>% summarise(TOT_SWITCH=sum(SWITCH)) %>% filter(TOT_SWITCH==0) %>% pull(FINREGISTRYID)

cat(
  'number of failures to 0 triptans (controls): ', length(CONTROL), 
  'in percentage: ',100*length(CONTROL)/ORIGINAL_N
)  

###############
####   4   ####
###############

# sanity-check
if( nrow(cohort) != SC ){ 
  print('failed sanity check: something is wrong!') 
}

# overwrite the file to add the case-control results
fwrite(cohort, file=study_population_file, append = FALSE)











