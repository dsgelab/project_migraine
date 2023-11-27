###############################################################################
#
# 1) import of main datasets 
#      dat: detailed longitudinal data R12
#      vnr: vnr codes
#      cov: analysis covariates R12
# 
# 2) calculate bday and death/end of fup dates and keep ID individuals alive on 01/01/2010
#
# 3) keep only medication purchases from dat (PURCH), filter purchases from 1998 and join vnr infos and write the file
#
###############################################################################


library(R.utils)
library(data.table)
library(dplyr)
library(lubridate)

#############
#     1     #
#############
dat <- fread('/finngen/library-red/finngen_R12/phenotype_1.0/data/finngen_R12_detailed_longitudinal_1.0.txt.gz')

vnr <- fread('/home/ivm/drugs/data/vnr_mapping_6_5.tsv')

cov <- fread('/finngen/library-red/finngen_R12/analysis_covariates/R12_COV_V2.FID.txt.gz')

#############
#     2     #
#############
bday<- dat%>% 
  arrange(APPROX_EVENT_DAY) %>%
  distinct(FINNGENID, .keep_all=TRUE) %>%
  mutate(bday=lubridate::as_date(APPROX_EVENT_DAY)-lubridate::dyears(EVENT_AGE)) %>%
  select(FINNGENID,bday)

cov<- left_join(cov,bday,by=c("IID"="FINNGENID"))%>% 
  mutate(dday=bday+lubridate::dyears(AGE_AT_DEATH_OR_END_OF_FOLLOWUP))

alive.2010 <- cov %>% 
  filter(dday>"2010-01-01")%>%
  select(FINNGENID=IID) %>%
  pull(FINNGENID)

vnr2 <- vnr[!duplicated(vnr), ]

#############
#     3     #
#############
purch <- dat %>% 
  filter(SOURCE=="PURCH",
         FINNGENID %in% alive.2010) %>%
  #cast event_day to Date, CODE3 to integer
  mutate(APPROX_EVENT_DAY= as.Date(APPROX_EVENT_DAY),
         VNR = as.integer(CODE3)) %>%
  #filter purchases from 1998 on
  filter(format(APPROX_EVENT_DAY,'%Y')>=1998) %>%
  #filter individuals in the cov_pheno file, for wich the GWAS will be run
  filter(FINNGENID %in% cov$FID) %>% 
  #merge with VNRs 
  left_join(vnr2, by=c('VNR'='vnr'))%>%
  select(-ICDVER,-CATEGORY)


nrow(purch)
length(unique(purch$FINNGENID))



fwrite(purch,'/home/ivm/drugs/data/finngen_R12_purch_vnr_98_alive_2010.gz', sep='\t', quote=F,
       compress="gzip", na="NA")

print("* * * * WRITTEN finngen_R12_purch_vnr_98_alive_2010.gz * * * *")





