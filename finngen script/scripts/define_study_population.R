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
# - remove zolmtriptan users (only in nasal spray form)
# - remove cluster migraine patients
# - is indexed person (alive and living in finland on 01.01.2010) 
# - only first purchase between 2000 and 2021 (remove prevalent users + time window bias) 
# - remove people with 1° purchase made before 18 yo and after 75 yo
# - remove people with only 1 purchase and with ATC=N02CC
# - remove people with < 5 purchases within 2 years of follow up 
#
# 3) export study cohort
# 
###############################################################################


# PREPARE ENVIRONMENT:
#NB: using ePouta shared env available packages

suppressPackageStartupMessages({
  
  library(data.table)
  library(dplyr)
  library(lubridate)
  
})
print('correctly set up the R environment')

source('/home/ivm/project_migraine/file_paths.R')


###############
####   1   ####
###############

# MAIN:
cohort <- fread(triptan_users_file)

cat('total number of purchases available: ')
cat(nrow(cohort),'\n')

cat('total number of triptan users: ')
ORIGINAL_N = length(unique(cohort$FINNGENID))
cat(ORIGINAL_N,'\n')

print('percentage of triptan users in FINNGEN:')
print(100*length(unique(cohort$FINNGENID))/520105)

unique(cohort$CODE1)

# EXTRA:
vnr <- fread(VNR_codes_file)
ep_cluster<-fread(cluster_ep_file)%>%
  rename(
    "AGE_DEATH_ENDFUP_EP"="AGE_AT_DEATH_OR_END_OF_FOLLOWUP"
  )

COV_OF_INTEREST = c(
  "IID",                  
  "AGE_AT_DEATH_OR_END_OF_FOLLOWUP",                     
  "DEATH_FU_AGE",                  
  "SEX",  
  "SEX_IMPUTED",
  "BMI",
  "movedabroad"               
)

cov <- fread(covariates_file) %>%  select(all_of(COV_OF_INTEREST))


# merge extra information:
cohort <- cohort %>% 
  left_join(cov, by=c("FINNGENID"="IID")) %>%
  rename(
    #"VNR_CODE"="CODE3",
    "ATC_CODE"="CODE1"
  ) %>%
  mutate(
    APPROX_EVENT_DAY= as.Date(APPROX_EVENT_DAY),
    VNR_CODE = as.integer(CODE3),
    PURCH_DATE = lubridate::as_date(APPROX_EVENT_DAY),
    BIRTH_DATE = lubridate::as_date(APPROX_EVENT_DAY)-lubridate::dyears(EVENT_AGE),
    bday=lubridate::as_date(APPROX_EVENT_DAY)-lubridate::dyears(EVENT_AGE),
    dday=bday+lubridate::dyears(AGE_AT_DEATH_OR_END_OF_FOLLOWUP),
  ) %>%
  mutate(INDEX_PERSON = ifelse(dday>"2010-01-01",1,0)) %>%
  left_join(vnr, by=c("VNR_CODE"="vnr")) %>% 
  left_join(ep_cluster, by="FINNGENID")


###############
####   2   ####
###############


# remove zolmitriptan users (only nasal spray, see the pkokok or vahvuus)  
N0 = length(unique(cohort$FINNGENID))
to_keep <- cohort %>% filter(!grepl("NASAL", valmiste, ignore.case = TRUE) &
                            !grepl("N02CC03", ATC_CODE, ignore.case = TRUE) &
                              !grepl("ML", pkoko, ignore.case = TRUE)) %>% pull(FINNGENID) %>% unique()
cohort <- cohort[cohort$FINNGENID %in% to_keep]
N1 = length(unique(cohort$FINNGENID))
print(paste(N0-N1,'zolmitriptan nasal spray users'))

#remove Cluster migraine patients
N0 = length(unique(cohort$FINNGENID))
to_keep <- cohort %>% filter(G6_CLUSTHEADACHE_WIDE==0 |  is.na(G6_CLUSTHEADACHE_WIDE) ) %>% pull(FINNGENID) %>% unique()
cohort <- cohort[cohort$FINNGENID %in% to_keep]
N1 = length(unique(cohort$FINNGENID))
print(paste(N0-N1,'Cluster migraine patients'))

# remove non indexed people 
N0 = length(unique(cohort$FINNGENID))
to_keep <- cohort %>% filter(INDEX_PERSON==1) %>% pull(FINNGENID) %>% unique()
cohort <- cohort[cohort$FINNGENID %in% to_keep]
N1 = length(unique(cohort$FINNGENID))
cat(paste(N0-N1,'non indexed people, eliminating',
          100*(N0-N1)/ORIGINAL_N,'% of original people \n'))


# remove patients with first purchase before 2000 or after 2021
N0 = length(unique(cohort$FINNGENID))
cohort <- cohort %>% 
  group_by(FINNGENID) %>%
  arrange(FINNGENID,APPROX_EVENT_DAY) %>%
  mutate(
    PURCH_N = row_number(),
    IS_FIRST_PURCH = ifelse(PURCH_N==1, 1, 0)
  ) %>% 
  ungroup() %>%
  mutate(YEAR_FIRST_PURCH = ifelse(IS_FIRST_PURCH==1, lubridate::year(PURCH_DATE), NaN),
         AGE_FIRST_PURCH = ifelse(IS_FIRST_PURCH==1, EVENT_AGE, NaN)) 

to_keep <- cohort %>%
  filter(YEAR_FIRST_PURCH>=2000 & YEAR_FIRST_PURCH<2021 ) %>%
  pull(FINNGENID)
cohort  <- cohort %>% filter(FINNGENID %in% to_keep)

N1 = length(unique(cohort$FINNGENID))
cat(paste(N0-N1,'patients with first purchase outside of [2000,2021], eliminating',
          100*(N0-N1)/ORIGINAL_N,'% of original people \n'))

# remove people with 1° purchase made before turning 18 or after turning 75
N0 = length(unique(cohort$FINNGENID))

to_keep <- cohort %>%
  filter(AGE_FIRST_PURCH>=18 & AGE_FIRST_PURCH<76 ) %>%
  pull(FINNGENID)
cohort  <- cohort %>% filter(FINNGENID %in% to_keep)

N1= length(unique(cohort$FINNGENID))

cat(paste(N0-N1,'patients with 1° purchases made before turning 18 or after turning 75, eliminating',
          100*(N0-N1)/ORIGINAL_N,'% of original people \n'))

# remove people with only 1 purchase
N0 = length(unique(cohort$FINNGENID))
to_keep <- cohort %>%
  group_by(FINNGENID) %>%
  summarize(TOT_PURCH=n()) %>%  
  filter(TOT_PURCH>1) %>%
  pull(FINNGENID)
cohort <- cohort[cohort$FINNGENID %in% to_keep,]
N1 = length(unique(cohort$FINNGENID))
cat(paste(N0-N1,'people with 1 purchase (after other inclusion criteria), eliminating',
          100*(N0-N1)/ORIGINAL_N,'% of original people \n'))

#remove patients with N02CC 
N0 = length(unique(cohort$FINNGENID))
to_drop <- cohort %>% filter(ATC_CODE=="N02CC") %>% pull(FINNGENID) %>% unique()
cohort <- cohort%>% filter(!(FINNGENID %in% to_drop))
N1 = length(unique(cohort$FINNGENID))
print(paste(N0-N1,'Patients with N02CC purchases'))



# Keep first 2 years from 1° purchase
N0 = length(unique(cohort$FINNGENID))
first_date<-cohort %>% group_by(FINNGENID) %>%  summarize(DATE_FIRST_PURCH = min(PURCH_DATE)) 

cohort <- cohort %>%
  left_join(first_date, by = "FINNGENID")%>%
  group_by(FINNGENID) %>%
  arrange(FINNGENID,PURCH_DATE) %>% 
  mutate(
    PURCH_N = row_number(),
    IS_FIRST_PURCH = ifelse(PURCH_N==1, 1, 0),
    FIRST2YRS = PURCH_DATE <= (DATE_FIRST_PURCH + 365 * 2)
  ) %>% 
  ungroup()  %>%  filter(FIRST2YRS==TRUE) 

rm(first_date)

table(cohort$ATC_CODE)


# keep only pat with >=5 purch (within first 2 yrs from 1° purch)
N0 = length(unique(cohort$FINNGENID))

to_keep <- cohort %>%
  group_by(FINNGENID) %>%
  summarize(TOT_PURCH=n()) %>%  
  filter(TOT_PURCH>=5) %>%
  pull(FINNGENID)

cohort <- cohort[cohort$FINNGENID %in% to_keep,]
N1 = length(unique(cohort$FINNGENID))
cat(paste(N0-N1,'people with <=5 triptan purchase within 2 years from 1° purchase \n eliminating',
          100*(N0-N1)/ORIGINAL_N,'% of original people \n'))

#merge NSAID purchases infos (after 1° triptan purchase and within 2yrs from it)
nsaid_cohort <- fread(nsaid_users_file) %>% filter (FINNGENID %in% cohort$FINNGENID) %>%
  mutate(
    APPROX_EVENT_DAY_NSAID= as.Date(APPROX_EVENT_DAY),
    PURCH_DATE_NSAID= lubridate::as_date(APPROX_EVENT_DAY_NSAID)
  ) %>% select(FINNGENID, APPROX_EVENT_DAY_NSAID)

First_trpt <- cohort %>% arrange(FINNGENID, PURCH_DATE) %>% group_by(FINNGENID) %>% slice(1) %>% select(FINNGENID, DATE_FIRST_PURCH)

merged_trpt_nsaid <- left_join(nsaid_cohort,First_trpt, by="FINNGENID") %>% 
                     filter((APPROX_EVENT_DAY_NSAID > DATE_FIRST_PURCH) & (APPROX_EVENT_DAY_NSAID <= (DATE_FIRST_PURCH + 365 * 2)))
  
merged_trpt_nsaid <- merged_trpt_nsaid %>% group_by(FINNGENID) %>% summarise(N_PURCH_NSAID=n())

cohort <- left_join(cohort, merged_trpt_nsaid, by="FINNGENID") %>% mutate(N_PURCH_NSAID=ifelse(is.na(N_PURCH_NSAID), 0, N_PURCH_NSAID))

rm(First_trpt, merged_trpt_nsaid, nsaid_cohort, ep_cluster)

###############
####   3   ####
###############

fwrite(cohort, file=study_population_file, append=FALSE)

cat('total number of people included in the study is: ')
FINAL_N = length(unique(cohort$FINNGENID))
cat(FINAL_N,'\n')
cat('percentage of the original population: ')
cat(100*FINAL_N/ORIGINAL_N)
