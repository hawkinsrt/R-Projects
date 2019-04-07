source('Custom Functions work in progress.R')

ImportLib(tidyverse)

claims <- read_rds('Data/claimsCleanSmall.RDS')

# add lead service type to claims. Creates claim_lead_age_group.csv
transform_claims(claims)

claims <- claims %>% 
  select(CLAIM_NUM,MRN_ALIAS, EPISODE_SEQ, MEMBER_SEX, SERVICE_TYPE)

suppressWarnings(lead<- read_csv("Data/claim_lead_age_group.csv", 
                                 col_types = cols(AGE_GROUP = col_character(), 
                                                  CLAIM_NUM = col_character(), LEAD_EPISODE = col_integer(), 
                                                  LEAD_YEAR = col_integer(), MRN_ALIAS = col_character(), 
                                                  ED_NEXT_3 = col_integer())))

ccs <- read_csv("Data/claim_ccs_category.csv", 
                col_types = cols(CLAIM_NUM = col_character(), 
                                 CODE_1 = col_character(), CODE_2 = col_character(), 
                                 CODE_3 = col_character(), CODE_4 = col_character(), 
                                 CODE_5 = col_character(), CODE_6 = col_character(), 
                                 CODE_7 = col_character()))

chronic <- read_csv("Data/claim_target_condition.csv", 
                    col_types = cols(Asthma = col_integer(), 
                                     CLAIM_NUM = col_character(), CLAIM_SEQ = col_character(), 
                                     COPD = col_integer(), Diabetes = col_integer(), 
                                     HeartFailure = col_integer(), Hypertension = col_integer(), 
                                     MRN_ALIAS = col_character(), MoodDisorder = col_integer(), 
                                     Psychoses = col_integer(), YEAR = col_integer()))

# join together
full <- claims %>% 
  left_join(lead, by =c('CLAIM_NUM','MRN_ALIAS')) %>% 
  left_join(ccs, by = 'CLAIM_NUM') %>% 
  left_join(chronic, by = c('CLAIM_NUM', 'MRN_ALIAS'))

full <- full %>% 
  select(MRN_ALIAS, MEMBER_SEX, AGE_GROUP, Asthma, COPD, Diabetes, HeartFailure,
         Hypertension, Psychoses, MoodDisorder, CLAIM_SEQ, EPISODE_SEQ, LEAD_EPISODE, SERVICE_TYPE,
         ED_NEXT_3, YEAR, LEAD_YEAR, everything())

# save
write_csv(full, 'Data/claims_ccs_as_code_with_lead_service.csv', col_names = TRUE)