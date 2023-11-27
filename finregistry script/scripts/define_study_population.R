###############################################################################
# 
# EXTRACT STUDY POPULATION:
# 
# 1) fetch data about triptan users
#
# TRIPTAN DEFINITION: 
# - ATC code starts with N02CC
# 
# 2) apply inclusion / exclusion criteria
# 
# RULES: 
# - is indexed person (alive and living in finland on 01.01.2010) 
# - only first purchase between 2000 and 2021 (remove prevalent users + time window bias) 
# - remove purchase made before 18 yo and after 75 yo
# - remove people with < 5 purchases 
#
# 3) export study cohort
# 
###############################################################################


# PREPARE ENVIRONMENT:
#NB: using ePouta shared env available packages

suppressPackageStartupMessages({
  
  library(data.table)
  library(dplyr)
  library(zoo)
  library(lubridate)
  library(ggplot2)
  
})

source('/data/projects/project_mferro/project_migraine/file_paths.R')


###############
####   1   ####
###############

# MAIN:
cohort <- fread(triptan_users_file)

cat('total number of purchases available: ')
cat(nrow(cohort),'\n')

cat('total number of triptan users: ')
ORIGINAL_N = length(unique(cohort$FINREGISTRYID))
cat(ORIGINAL_N,'\n')

cat('percentage of triptan users in finregistry: ')
cat(100*ORIGINAL_N/7166416,'\n')

# EXTRA:
vnr <- fread(VNR_codes_file)

COV_OF_INTEREST = c(
"FINREGISTRYID",                  
"INDEX_PERSON",                   
"DATE_OF_BIRTH",                  
"SEX",                            
"DEATH_DATE",                     
"MOTHER_TONGUE",                  
"EVER_MARRIED",                   
"EVER_DIVORCED",                  
"EMIGRATION_DATE",                
"EMIGRATED",                      
"NUMBER_OF_CHILDREN"
)

cov <- fread(covariates_file) %>%
  select(all_of(COV_OF_INTEREST))

# merge extra information:
cohort <- cohort %>% 
  left_join(cov, by="FINREGISTRYID") %>%
  left_join(vnr, by=c("CODE3"="VNRO")) %>%
  rename(
    "VNR_CODE"="CODE3",
    "ATC_CODE"="CODE1"
  ) %>%
  mutate(
    PURCH_DATE = lubridate::as_date(PVM),
    BIRTH_DATE = lubridate::as_date(DATE_OF_BIRTH),
    DEATH_DATE = lubridate::as_date(DEATH_DATE)
  ) %>%
  select(-c(PVM,DATE_OF_BIRTH))

###############
####   2   ####
###############

# remove non indexed people
N0 = length(unique(cohort$FINREGISTRYID))
to_keep <- cohort %>% filter(INDEX_PERSON==1) %>% pull(FINREGISTRYID) %>% unique()
cohort <- cohort[cohort$FINREGISTRYID %in% to_keep]
N1 = length(unique(cohort$FINREGISTRYID))
cat(paste(N0-N1,'non indexed people, eliminating',
          100*(N0-N1)/ORIGINAL_N,'% of original people \n'))


# remove patients with first purchase:
# before 2000 (prevalent users) 
# after 2021 (?)
N0 = length(unique(cohort$FINREGISTRYID))
cohort <- cohort %>% 
  group_by(FINREGISTRYID) %>%
  arrange(FINREGISTRYID,EVENT_DAY) %>%
  mutate(
    PURCH_N = row_number(),
    IS_FIRST_PURCH = ifelse(PURCH_N==1, 1, 0)
    ) %>% 
  ungroup() %>%
  mutate(YEAR_FIRST_PURCH = ifelse(IS_FIRST_PURCH==1, lubridate::year(PURCH_DATE), NaN)) 

to_keep <- cohort %>%
  filter(YEAR_FIRST_PURCH>=2000 & YEAR_FIRST_PURCH<2021 ) %>%
  pull(FINREGISTRYID)
cohort <- cohort[cohort$FINREGISTRYID %in% to_keep,]

N1 = length(unique(cohort$FINREGISTRYID))
cat(paste(N0-N1,'patients with first purchase outside of [2000,2021], eliminating',
          100*(N0-N1)/ORIGINAL_N,'% of original people \n'))

# remove purchases made before turning 18 or after turning 75
N0 = nrow(cohort)
cohort <- cohort %>%
  mutate(AGE_AT_PURCH = (PURCH_DATE-BIRTH_DATE)/365.25) %>%
  filter(AGE_AT_PURCH>=18 & AGE_AT_PURCH<75)
N1 = nrow(cohort)
cat(paste(N0-N1,'purchases made before turning 18 or after turning 75 \n'))

# remove people with only 1 purchase
N0 = length(unique(cohort$FINREGISTRYID))
to_keep <- cohort %>%
  group_by(FINREGISTRYID) %>%
  summarize(TOT_PURCH=n()) %>%
  filter(TOT_PURCH>1) %>%
  pull(FINREGISTRYID)
cohort <- cohort[cohort$FINREGISTRYID %in% to_keep,]
N1 = length(unique(cohort$FINREGISTRYID))
cat(paste(N0-N1,'people with 1 purchase (after other inclusion criteria), eliminating',
          100*(N0-N1)/ORIGINAL_N,'% of original people \n'))

###############
####   3   ####
###############

fwrite(cohort, file=study_population_file, append=FALSE)

cat('total number of people included in the study is: ')
FINAL_N = length(unique(cohort$FINREGISTRYID))
cat(FINAL_N,'\n')
cat('percentage of the original population: ')
cat(100*FINAL_N/ORIGINAL_N)
