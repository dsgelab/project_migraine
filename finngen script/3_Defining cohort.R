
# Defining cohort of triptan users with incl/exlc criteria

###############################################################################
#
# 1) import of trajectories dataset  and apply the inclusion criteria (18<=AGE purch<=75)
#      nsaid75: NSAIDs purchases 
#      d.lt75 : triptan purchases 
#
#   1.1) Find triptan users who have also a NSAIDs purchase and merge the purchases info and then calculate:
#       a) Date of 1° triptan purchase (excluding N02CC03) , so filter only for purchases after this date 
#       b) For every purchase, if the date is before or after 2 years from 1° triptan purchase
#       c) If a patient used a NSAID after 6 months of 1° triptan purchase within 2 yrs from 1° purch or a nasal spray (N02CC03) after 1° triptan purchase within 2 yrs from 1° 
#
# 2) Merge all infos, apply exclusion criteria: 
#     - Delete prevalent users (those who have a purchase before 20000)
#     - Delete patients with <5 purchases (triptan+NSAIDs)
#    DEFINE Failure 1/2/3 triptans and Controls (dataset: trpt_failure2)
#
#
# 3) write dataset
#
###############################################################################


#############
#     1     #
#############

nsaid75 <-fread('drugs/data/R12_NSAIDs_98_23.txt') %>% filter(EVENT_AGE>=18) %>% 
  filter(EVENT_AGE <= 75) %>% select(FINNGENID, APPROX_EVENT_DAY, CODE1) 

d.lt75 <- fread('drugs/data/R12_triptans_98_23.txt') %>%   filter(EVENT_AGE <= 75) %>% filter(EVENT_AGE>=18) 

tript_db <- d.lt75 %>% arrange(FINNGENID, APPROX_EVENT_DAY) %>% filter(EVENT_AGE>=18) %>% 
  select(FINNGENID, APPROX_EVENT_DAY, CODE1)
#############
#   1.1     #
#############

id_tript <- tript_db %>%  distinct(FINNGENID)

nsaid_df_filtered <- nsaid75 %>%  filter(FINNGENID %in% id_tript$FINNGENID)

merged_df <- rbind(tript_db,nsaid_df_filtered)

merged_df <- merged_df %>%  arrange(FINNGENID, APPROX_EVENT_DAY)

#############
#  1.1 a)   #
#############

prima_data_triptano <- merged_df %>%
  filter(substr(CODE1, 1, 5) == "N02CC" & CODE1!="N02CC03") %>% #escludo il caso in cui la 1° purch è nasal spray
  group_by(FINNGENID) %>%
  summarize(prima_data_triptano = min(APPROX_EVENT_DAY))

merged_df <- merged_df %>%
  left_join(prima_data_triptano, by = "FINNGENID")

#############
#  1.1 b)   #
#############

merged_df_trp_ns <- merged_df %>%
  filter(is.na(prima_data_triptano) | APPROX_EVENT_DAY >= prima_data_triptano)%>%
  mutate(first2yrs = APPROX_EVENT_DAY <= (prima_data_triptano + 365 * 2)) %>% # check su FUP (almeno 2 anni? o <=2anni?)
  mutate(nsaid_after6m =  substr(CODE1, 1, 4) == "M01A" & APPROX_EVENT_DAY > (prima_data_triptano + 180)  ) %>%
  mutate(nasal_spray = (CODE1 == "N02CC03"))  

table(merged_df_trp_ns$nsaid_after6m)
table(merged_df_trp_ns$nasal_spray)
table(merged_df_trp_ns$CODE1)


# tengo i primi due anni dalla prima prescrizione di triptano

merged_df2 <- merged_df_trp_ns %>%  filter(is.na(prima_data_triptano) | first2yrs)

n_presc_trpt_nsaid <- merged_df2 %>% group_by(FINNGENID) %>% summarize(numero_prescrizioni = n()) %>% ungroup()


#############
#  1.1 c)   #
#############

# salvo i pz che hanno almeno un nsaid nei due anni AND dopo 6 mesi dal 1° trpt 
pazienti_nsaid <- merged_df2 %>%
  group_by(FINNGENID) %>%
  summarize(has_used_nsaid = any(nsaid_after6m))

pazienti_naspray <- merged_df2 %>%
  group_by(FINNGENID) %>%
  summarize(has_used_nasalspray= any(nasal_spray))




#############
#     2     #
#############

trpt_failure <-  d.lt75 %>%
  left_join(prima_data_triptano, by = "FINNGENID")

trpt_failure <-  trpt_failure %>%
  left_join(pazienti_nsaid, by = "FINNGENID")

trpt_failure <-  trpt_failure %>%
  left_join(pazienti_naspray, by = "FINNGENID")

trpt_failure <-  trpt_failure %>%
  left_join(n_presc_trpt_nsaid, by = "FINNGENID")


trpt_failure2<- trpt_failure %>% arrange(FINNGENID, APPROX_EVENT_DAY) %>% 
  filter(year(prima_data_triptano)>=2000) %>%
  filter (APPROX_EVENT_DAY >= (prima_data_triptano)) %>% 
  filter (APPROX_EVENT_DAY <= (prima_data_triptano + 365 * 2)) %>%
  filter (numero_prescrizioni>=5) %>% 
  group_by(FINNGENID) %>% 
  mutate(VISIT_NUMBER = row_number()) %>%
  
  mutate(F1 = ifelse(row_number() == 2 & CODE1 != lag(CODE1) & CODE1 == lead(CODE1) & CODE1 == lead(CODE1, 2), 1, 0))  %>% 
  mutate(F2 = ifelse(row_number() == 1 & CODE1 != lead(CODE1)
                     & lead(CODE1) != lead(CODE1, 2) & lead(CODE1, 2) == lead(CODE1, 3) & lead(CODE1, 3) == lead(CODE1, 4), 1, 0)) %>%
  mutate(F3 = ifelse ((row_number() == 1 & CODE1 != lead(CODE1) & 
                         lead(CODE1) != lead(CODE1, 2) & lead(CODE1, 2) != lead(CODE1, 3)), 1, 0)) %>%
  
  mutate(control = ifelse((row_number() == 1 & CODE1 == lead(CODE1) & CODE1 == lead(CODE1,2) & CODE1 == lead(CODE1,3)), 1, 0)) %>% 
  
  # Redefine F1 who have NSAID after 6 months from 1° purch --> To check definition about it
  mutate(F1_nsaid = ifelse((row_number() == 1 & has_used_nsaid==1), 1, 0)) %>%
  mutate(F1_tot = ifelse((row_number() == 1 & F1_nsaid==1 & is.na(F2) & is.na(F3) | 
                            (row_number() == 1 & F1_nsaid==1 & control==1) ), 1, 0)) %>%
  #reclassify controls 
  mutate(control2 = ifelse((row_number() == 1  & F1_tot == 0 & control==1), 1, 0)) %>% 
  ungroup()

length(unique(trpt_failure2$FINNGENID))

table(trpt_failure2$F1)
table(trpt_failure2$F2)
table(trpt_failure2$F3)
table(trpt_failure2$control)

#############
#     3     #
#############
fwrite(trpt_failure2, 'drugs/data/fin_cohort_00_23.txt', sep = '\t', quote = F)














