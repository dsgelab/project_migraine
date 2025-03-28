
####################################################
#
# 1. fetch data 
# 2. define cases
# 3. define controls
# 4. excluding patients that doesn't fit our definition of cases and controls
# 5. save results
# 
####################################################

# PREPARE ENVIRONMENT:
#NB: using ePouta shared env available packages

suppressPackageStartupMessages({
  
  library(data.table)
  library(dplyr)
  library(lubridate)
  
})

source('/home/ivm/project_migraine/file_paths.R')

###############
####   1   ####
###############

cohort <- fread(study_population_file)

# for later sanity-checks
SC <- nrow(cohort)
ORIGINAL_N <- length(unique(cohort$FINNGENID))

###############
####   2   ####
###############

# define switch 
cohort <- cohort %>% arrange(FINNGENID, APPROX_EVENT_DAY) %>%
  group_by(FINNGENID) %>%
  mutate(SWITCH = ifelse(ATC_CODE != lag(ATC_CODE),1,0)) %>%
  mutate(SWITCH = ifelse(is.na(SWITCH),0,SWITCH)) %>%

mutate(VISIT_NUMBER = row_number()) %>%
  
  mutate(F1 = ifelse(row_number() == 1 & ATC_CODE != lead(ATC_CODE) & lead(ATC_CODE) == lead(ATC_CODE,2) & lead(ATC_CODE,2) == lead(ATC_CODE, 3), 1, 0))  %>% 
  mutate(F2 = ifelse(row_number() == 1 & ATC_CODE != lead(ATC_CODE)
                     & lead(ATC_CODE) != lead(ATC_CODE, 2) & lead(ATC_CODE, 2) == lead(ATC_CODE, 3) & lead(ATC_CODE, 3) == lead(ATC_CODE, 4), 1, 0)) %>%
  mutate(F3 = ifelse ((row_number() == 1 & ATC_CODE != lead(ATC_CODE) & 
                         lead(ATC_CODE) != lead(ATC_CODE, 2) & lead(ATC_CODE, 2) != lead(ATC_CODE, 3)), 1, 0)) %>%
  
  mutate(control_e = ifelse((row_number() == 1 & ATC_CODE == lead(ATC_CODE) & ATC_CODE == lead(ATC_CODE,2) & ATC_CODE == lead(ATC_CODE,3)), 1, 0))

# QC:
# table( cohort %>% summarise(TOT_SWITCH=sum(SWITCH)) %>% pull(TOT_SWITCH) )


TRPT_FAIL_1 <- cohort %>% summarise(TOT_SWITCH=sum(SWITCH)) %>% filter(TOT_SWITCH==1) %>% pull(FINNGENID)
TRPT_FAIL_2 <- cohort %>% summarise(TOT_SWITCH=sum(SWITCH)) %>% filter(TOT_SWITCH==2) %>% pull(FINNGENID)
TRPT_FAIL_3 <- cohort %>% summarise(TOT_SWITCH=sum(SWITCH)) %>% filter(TOT_SWITCH>=3) %>% pull(FINNGENID)

F1 <- cohort %>% filter(F1==1) %>% pull(FINNGENID)
F2 <- cohort %>% filter(F2==1) %>% pull(FINNGENID)
F3 <- cohort %>% filter(F3==1) %>% pull(FINNGENID)


cat( '****naive definition*** \n',
  'number of failures to 1 triptan: ', length(TRPT_FAIL_1), 
  'in percentage: ',100*length(TRPT_FAIL_1)/ORIGINAL_N,'\n',
  'number of failures to 2 triptan: ', length(TRPT_FAIL_2), 
  'in percentage: ',100*length(TRPT_FAIL_2)/ORIGINAL_N,'\n',
  'number of failures to >= 3 triptan: ', length(TRPT_FAIL_3), 
  'in percentage: ',100*length(TRPT_FAIL_3)/ORIGINAL_N,'\n \n',
  '****Based on first 5 purchases*** \n',
  'number of failures to 1 triptan: ', length(F1), 
  'in percentage: ',100*length(F1)/ORIGINAL_N,'\n',
  'number of failures to 2 triptan: ', length(F2), 
  'in percentage: ',100*length(F2)/ORIGINAL_N,'\n',
  'number of failures to >= 3 triptan: ', length(F3), 
  'in percentage: ',100*length(F3)/ORIGINAL_N,'\n'
)


###############
####   3   ####
###############

CONTROL <- cohort %>% summarise(TOT_SWITCH=sum(SWITCH)) %>% filter(TOT_SWITCH==0) %>% pull(FINNGENID)
CTRL <- cohort %>% filter(control_e==1) %>% pull(FINNGENID)

cat('****naive definition*** \n',
  'number of failures to 0 triptans (controls): ', length(CONTROL), 
  'in percentage: ',100*length(CONTROL)/ORIGINAL_N,'\n',
  '****Based on first 5 purchases*** \n',
  '>=3 triptans purchases without switching controls): ', length(CTRL), 
  'in percentage: ',100*length(CTRL)/ORIGINAL_N
)  

###############
####  3.1  ####
###############

# sanity-check 
if( nrow(cohort) != SC ){ 
  print('failed sanity check: something is wrong!') 
}

###############
####   4   ####
###############
to_keep<- cohort %>% filter(F1==1 | F2==1 | F3==1 | control_e==1) %>% pull(FINNGENID) %>% unique()
cohort <- cohort %>% filter(FINNGENID %in% to_keep)
length(unique(cohort$FINNGENID))

###############
####   5   ####
###############

# overwrite the file to add the case-control results
fwrite(cohort, file=study_population_file, append = FALSE)


