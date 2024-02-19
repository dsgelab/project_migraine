
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

# define switch based on first definition (official)
cohort <- cohort %>% 
  group_by(FINREGISTRYID) %>%
  arrange(FINREGISTRYID, PVM) %>%
  mutate(SWITCH = ifelse(ATC_CODE != lag(ATC_CODE),1,0)) %>%
  mutate(SWITCH = ifelse(is.na(SWITCH),0,SWITCH)) %>%
  mutate(VISIT_NUMBER = row_number()) %>%  
  mutate(SWITCH_1 = ifelse((
    row_number() == 1 & 
    ATC_CODE != lead(ATC_CODE) & 
    lead(ATC_CODE) == lead(ATC_CODE,2) & 
    lead(ATC_CODE,2) == lead(ATC_CODE,3)),
    1, 0)
  )  %>% 
  mutate(SWITCH_2 = ifelse((
    row_number() == 1 & 
    ATC_CODE != lead(ATC_CODE) &
    lead(ATC_CODE) != lead(ATC_CODE,2) & 
    lead(ATC_CODE,2) == lead(ATC_CODE,3) & 
    lead(ATC_CODE,3) == lead(ATC_CODE,4)),
    1, 0)
  ) %>%
  #NB: switch 3 or more times
  mutate(SWITCH_3 = ifelse((
    row_number() == 1 &
    ATC_CODE != lead(ATC_CODE) &
    lead(ATC_CODE) != lead(ATC_CODE,2) &
    lead(ATC_CODE,2) != lead(ATC_CODE,3)),
    1, 0)
  ) %>%  
  mutate(NO_SWITCH = ifelse((
    row_number() == 1 &
    ATC_CODE == lead(ATC_CODE) &
    ATC_CODE == lead(ATC_CODE,2) &
    ATC_CODE == lead(ATC_CODE,3)), 
    1, 0)
  )


# define switch based on second definition (NOT official)
TRPT_FAIL_1 <- cohort %>% summarise(TOT_SWITCH=sum(SWITCH)) %>% filter(TOT_SWITCH==1) %>% pull(FINREGISTRYID)
TRPT_FAIL_2 <- cohort %>% summarise(TOT_SWITCH=sum(SWITCH)) %>% filter(TOT_SWITCH==2) %>% pull(FINREGISTRYID)
TRPT_FAIL_3 <- cohort %>% summarise(TOT_SWITCH=sum(SWITCH)) %>% filter(TOT_SWITCH>=3) %>% pull(FINREGISTRYID)

F1 <- cohort %>% filter(SWITCH_1==1) %>% pull(FINREGISTRYID)
F2 <- cohort %>% filter(SWITCH_2==1) %>% pull(FINREGISTRYID)
F3 <- cohort %>% filter(SWITCH_3==1) %>% pull(FINREGISTRYID)

cat( ' ----- naive definition ----- \n',
     'number of failures to 1 triptan: ', length(TRPT_FAIL_1), 
     'in percentage: ',100*length(TRPT_FAIL_1)/ORIGINAL_N,'\n',
     'number of failures to 2 triptan: ', length(TRPT_FAIL_2), 
     'in percentage: ',100*length(TRPT_FAIL_2)/ORIGINAL_N,'\n',
     'number of failures to >= 3 triptan: ', length(TRPT_FAIL_3), 
     'in percentage: ',100*length(TRPT_FAIL_3)/ORIGINAL_N,'\n \n',
     ' ----- Based on first 5 purchases ----- \n',
     'number of failures to 1 triptan: ', length(F1), 
     'in percentage: ',100*length(F1)/ORIGINAL_N,'\n',
     'number of failures to 2 triptan: ', length(F2), 
     'in percentage: ',100*length(F2)/ORIGINAL_N,'\n',
     'number of failures to >= 3 triptan: ', length(F3), 
     'in percentage: ',100*length(F3)/ORIGINAL_N,'\n','\n'
)


###############
####   3   ####
###############

CONTROL <- cohort %>% summarise(TOT_SWITCH=sum(SWITCH)) %>% filter(TOT_SWITCH==0) %>% pull(FINREGISTRYID)
CTRL <- cohort %>% filter(NO_SWITCH==1) %>% pull(FINREGISTRYID)

cat(' ----- naive definition ----- \n',
    'number of failures to 0 triptans (controls): ', length(CONTROL), 
    'in percentage: ',100*length(CONTROL)/ORIGINAL_N,'\n','\n',
    ' ----- Based on first 5 purchases ----- \n',
    '>=3 triptans purchases without switching controls): ', length(CTRL), 
    'in percentage: ',100*length(CTRL)/ORIGINAL_N,'\n','\n'
)  


###############
####   4   ####
###############

# sanity-check
if( nrow(cohort) != SC ){ 
  print('failed sanity check: something is wrong!') 
}

# keep only official definition
to_keep<- cohort %>% 
  filter(SWITCH_1==1 | SWITCH_2==1 | SWITCH_3==1 | NO_SWITCH==1) %>% 
  pull(FINREGISTRYID) %>% 
  unique()
cohort <- cohort %>% filter(FINREGISTRYID %in% to_keep)
N = length(unique(cohort$FINREGISTRYID))
cat(paste0(
  'starting with a total of ',ORIGINAL_N,' patient purchase trajectories','\n',
  'finishing with a total of ',N,' patients','\n',
  'removing ',(ORIGINAL_N-N),' trajectories non compliant to the definition'
))


# overwrite the file to add the case-control results
fwrite(cohort, file=study_population_file, append = FALSE)











