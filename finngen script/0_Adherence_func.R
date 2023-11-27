# # # Functions to extract individual purchasing trajectories, summarise those 
# # # and derive adherence and persistence phenotypes

require(data.table)
require(dplyr)
require(lubridate)


# Exctract individual level purchase trajectories using event date
getTrajectoriesDates <- function(purch,ATCs,dose=1) {
  # ATCs as a regex string (e.g. '^C10AA' for statins) 
  df <- purch %>%
    filter(grepl(ATCs, CODE1)) %>%
    group_by(FINNGENID) %>%
    # remove rows duplicated because they have different INDEX values in the registries
    select(-INDEX, -SOURCE, -ATC) %>%
    distinct() %>%
    # filter trajectories with at least 4 purchases
    filter(n()>=4) %>%
    # filter trajectories with VNR info (package size) for all the events
    filter(all(!is.na(pkoko_num))) %>%
    # for each individual, order by event_age
    mutate(APPROX_EVENT_DAY = as.Date(APPROX_EVENT_DAY)) %>% 
    arrange(APPROX_EVENT_DAY, .by_group = T)  %>%
    mutate(# compute difference between purchases
      days_next_purch = as.integer(round( (lead(APPROX_EVENT_DAY) - APPROX_EVENT_DAY))),
      # calculate n pills for each purchase, imputing when n pkg is 0 (e.g. 238 days, 100 pills: 238 %% 100 = 38, n_pills = 200)
      # adjusting specific dose
      n_pills = case_when(CODE4 == 0 ~ (days_next_purch - (days_next_purch %% pkoko_num))/dose,
                          TRUE ~ pkoko_num*CODE4/dose)) %>%
    filter(days_next_purch != 0 | is.na(days_next_purch)) %>%
    mutate(
      change_type = CODE1 != lag(CODE1),
      # adjust n pills: if I change formulation after e.g. 20 days and I've bought 100 pills, I count only 20 pills for that purch
      n_pills = case_when(lead(change_type) == TRUE & days_next_purch <= n_pills ~ days_next_purch,
                          TRUE ~ n_pills),
      # identify gap >=150 days without pills
      gap = ifelse(days_next_purch > n_pills+180, 1, 0),
      # set n_pills to NA for last purchase and 'gap' purchases (where days_next is.na)
      gap_days = ifelse(gap == 1, days_next_purch - n_pills, 0),
      # set days to NA for gaps
      days_next_purch = case_when(gap == 1 ~ NA_real_,
                                  TRUE ~ days_next_purch),
      # set n_pills to NA for last purchase and 'gap' purchases (where days_next is.na)
      n_pills = case_when(is.na(days_next_purch) ~ NA_real_,
                          TRUE ~ n_pills),
      pills_norm = n_pills/days_next_purch,
      days_norm = days_next_purch/n_pills)
  return(df)
}

getTrajectoriesDates2 <- function(purch,ATCs) {
  # ATCs as a df with daily dose of each ATC
  df <- purch %>%
    inner_join(ATCs, by = c("CODE1"="atc")) %>%
    group_by(FINNGENID) %>%
    # remove rows duplicated because they have different INDEX values in the registries
    select(-INDEX) %>%
    distinct() %>%
    # filter trajectories with at least four purchases
    filter(n()>=4) %>%
    # filter trajectories with VNR info (package size) for all the events
    filter(all(!is.na(pkoko_num))) %>%
    mutate(APPROX_EVENT_DAY = as.Date(APPROX_EVENT_DAY)) %>% 
    # for each individual, order by event_age
    arrange(APPROX_EVENT_DAY, .by_group = T)  %>%
    # calculate n pills for each purchase, adjusting for the dose
    mutate(
      # compute difference between purchases
      days_next_purch = as.integer(round( (lead(APPROX_EVENT_DAY) - APPROX_EVENT_DAY))),
      # calculate n pills for each purchase, imputing when n pkg is 0 (e.g. 238 days, 100 pills: 238 %% 100 = 38, n_pills = 200)
      # adjusting specific doses
      n_pills = case_when(CODE4 == 0 ~ (days_next_purch - (days_next_purch %% pkoko_num))/dose,
                          TRUE ~ pkoko_num*CODE4/dose)) %>%
    filter(days_next_purch != 0 | is.na(days_next_purch)) %>%
    mutate(
      change_type = CODE1 != lag(CODE1),
      # adjust n pills: if I change formulation after e.g. 20 days and I've bought 100 pills, I count only 20 pills for that purch
      n_pills = case_when(lead(change_type) == TRUE & days_next_purch <= n_pills ~ days_next_purch,
                          TRUE ~ n_pills),
      # identify gap >=150 days without pills
      gap = ifelse(days_next_purch > n_pills+150, 1, 0),
      # set n_pills to NA for last purchase and 'gap' purchases (where days_next is.na)
      gap_days = ifelse(gap == 1, days_next_purch - n_pills, 0),
      # set days to NA for gaps
      days_next_purch = case_when(gap == 1 ~ NA_real_,
                                  TRUE ~ days_next_purch),
      # set to NA last purchase and 'gap' purchases (where days_nest is.na)
      n_pills = case_when(is.na(days_next_purch) ~ NA_real_,
                          TRUE ~ n_pills),
      pills_norm = n_pills/days_next_purch,
      days_norm = days_next_purch/n_pills)
  return(df)
}


summarizeTrajectories <- function(traj, min_age = 0, thr = 1.1, atc = "AAAAAAA") {
  t <- traj %>%
    group_by(FINNGENID) %>%
    summarise(tot_pills = sum(n_pills, na.rm = T),
              tot_days = sum(days_next_purch, na.rm = T),
              mean_days = mean(days_next_purch, na.rm = T),
              sd_days = sd(days_next_purch, na.rm = T),
              tot_purch = length(which(!is.na(n_pills))),
              age_first_purch = first(EVENT_AGE),
              n_break = sum(gap, na.rm = T),
              atc = atc,
              prop_atc = sum(CODE1 == atc)/n()) %>%
    mutate(adherence = tot_pills/tot_days) %>%
    # filter out trajectories which result to have NA for the previous metrics (e.g. if there is one purchase left)
    filter_all(all_vars(!is.na(.))) %>%
    # filter out outliers
    filter(adherence <= thr,
           age_first_purch >= min_age,
           tot_days >= 365) %>%
    mutate(age_bin = cut(age_first_purch, breaks = c(18, 48, 60, 80, Inf), labels = c("18-39", "40-59", "60-79", "80+"), right= FALSE)) %>%
    mutate(adherence_std = as.numeric(scale(adherence)))
}



getTrajectoriesPersistence <- function(df) {
  # Define "persistent" users:
  # - at least one year treatment
  # - without breaks
  # - no overbuyers (adherence < 1.1))
  df <- df %>% 
    filter(tot_days >= 365) %>% 
    mutate(persistent = 1) %>% 
    select(FINNGENID, tot_pills, tot_days, age_first_purch, age_bin, atc, prop_atc, persistent)
  return(df)
}


# Extract early stopping purchases
getTrajectoriesDiscontinuation <- function(purch,ATCs,cov,dose=1,min_age) {
  # Define early stopping users
  df <- purch %>%
    filter(grepl(ATCs, CODE1)) %>%
    # remove rows duplicated because they have different INDEX values in the registries
    select(-INDEX) %>%
    distinct() %>%
    left_join(cov, by = c("FINNGENID" = "IID")) %>% 
    group_by(FINNGENID) %>%
    # filter trajectories with VNR info (package size) for all the events
    filter(all(!is.na(pkoko_num))) %>%
    # for each individual, order by event_date
    arrange(APPROX_EVENT_DAY, .by_group = T)  %>%
    # keep trajectories with only one purchase at least 2 years before end of followup (EOF = death OR emigration OR 2020-01-01)
    filter(n()==1) %>%
    mutate(END_OF_FOLLOWUP = AGE_AT_DEATH_OR_END_OF_FOLLOWUP) %>%
    filter((END_OF_FOLLOWUP - EVENT_AGE) > 2) %>%
    # calculate n pills for each purchase
    mutate(tot_pills = pkoko_num*CODE4/dose,
           persistent = 0,
           tot_days = NA,
           tot_purch = 1,
           persistent = 0,
           atc = unique(CODE1),
           prop_atc = 1) %>%
    mutate(age_first_purch = EVENT_AGE,
           age_bin = cut(age_first_purch, breaks = c(18, 48, 60, 80, Inf), labels = c("18-39", "40-59", "60-79", "80+"), right= FALSE)) %>%
    filter(age_first_purch >= min_age) %>%
    select(FINNGENID, tot_pills, tot_days, age_first_purch, age_bin, atc, prop_atc, persistent)
  return(df)
}


getTrajectoriesDiscontinuation2 <- function(purch,ATCs,cov,min_age) {
  # ATCs as a df with daily dose of each ATC
  df <- purch %>%
    inner_join(ATCs, by = c("CODE1"="atc")) %>%
    left_join(cov, by = c("FINNGENID" = "IID")) %>% 
    group_by(FINNGENID) %>%
    # filter trajectories with VNR info (package size) for all the events
    filter(all(!is.na(pkoko_num))) %>%
    # for each individual, order by event_date
    arrange(APPROX_EVENT_DAY, .by_group = T)  %>%
    # keep trajectories with only one purchase at least 2 years before end of followup (EOF = death OR emigration OR 2020-01-01)
    filter(n()==1) %>%
    mutate(END_OF_FOLLOWUP = AGE_AT_DEATH_OR_END_OF_FOLLOWUP) %>%
    filter((END_OF_FOLLOWUP - EVENT_AGE) > 2) %>%
    # calculate n pills for each purchase
    mutate(tot_pills = pkoko_num*CODE4/dose,
           persistent = 0,
           tot_days = NA,
           tot_purch = 1,
           persistent = 0,
           atc = unique(CODE1),
           prop_atc = 1) %>%
    mutate(age_first_purch = EVENT_AGE,
           age_bin = cut(age_first_purch, breaks = c(18, 48, 60, 80, Inf), labels = c("18-39", "40-59", "60-79", "80+"), right= FALSE)) %>%
    filter(age_first_purch >= min_age) %>%
    select(FINNGENID, tot_pills, tot_days, age_first_purch, age_bin, atc, prop_atc, persistent)
  return(df)
}


# Get age of onset of the first of related endpoints
getAgeFirstEndpoint <- function(dat,ids,endpoints) {
  df <- dat 
  # Discard endpoints not present in the file
  endpoints <- intersect(endpoints, colnames(df)) 
  # Attach _age to get the age of onset columns
  endpoints_age <- paste0(endpoints, '_AGE')
  d <- df %>%
    select(FINNGENID, all_of(endpoints), all_of(endpoints_age)) %>%
    filter_at(vars(-FINNGENID), any_vars(.==1)) %>%
    select(FINNGENID, all_of(endpoints_age))
  # For each row, the min accross columns is the age of onset of the first of the events
  min_age <- apply(d[,2:length(endpoints)], 1, min)
  d <- d %>%
    mutate(age_first_ev = as.numeric(min_age)) %>%
    select(FINNGENID, age_first_ev)
  return(d)
}