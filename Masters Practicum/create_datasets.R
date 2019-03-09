source('Custom Functions work in progress.R')

# custom fucntion that downloads and install library if you don't have them, loads it if you do
ImportLib(tidyverse)

# Create chronic condition dataset
source('Chronic_Condition2.R')

# remove OTPT claims associated with ED visits, and duplicate claim numbers. Creates claimsCleanSmall.RDS
#createClaimsSmall()

claims <- read_rds('Data/claimsCleanSmall.RDS')

# add lead service type to claims. Creates claim_lead_age_group.rds
transform_claims(claims)

# turn diagnosis code into body_system category or css_category. Creates 5 temp rds files
# body_system for just body_system, css for just css_cateory or both
transform_codes(claims, 'ccs')

# read back in claims now that we don't need the space
#claims <- read_rds("Data/claimsCleanSmall.RDS")

# load lead service dataset and body system
claims <- claims %>% 
  select(CLAIM_NUM,MRN_ALIAS, EPISODE_SEQ, MEMBER_SEX, SERVICE_TYPE)

suppressWarnings(lead<- read_csv("Data/claim_lead_age_group.csv", 
                col_types = cols(AGE_GROUP = col_character(), 
                                 CLAIM_NUM = col_character(), LEAD_EPISODE = col_integer(), 
                                 LEAD_YEAR = col_integer(), MRN_ALIAS = col_character(), 
                                 NEXT_SERVICE = col_character())))

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
         NEXT_SERVICE, YEAR, LEAD_YEAR, everything())

# save
write_csv(full, 'Data/claims_ccs_as_code_with_lead_service.csv', col_names = TRUE)

rm(claims,ccs, lead, chronic)
gc()

# now remove the code columns and join ccs codes instead, this creates a second data set run models on
# because ccs is from VP it might be more accurate but does include many more factor levels

# claims <- read_rds('Data/claimsCleanSmall.RDS')
# transform_codes(claims, 'ccs')
# rm(claims)
# 
# full <- read_csv('Data/claims_ccs_as_code_with_lead_service.csv')

# full <- full %>% 
#   select(-starts_with('CODE_'))
# 
# body_system <- read_rds('Data/claim_body_system.csv')
# 
# full <- full %>% 
#   left_join(body_system, by = 'CLAIM_NUM')
# 
# write_csv(full, 'Data/claims_body_as_code_with_lead_service.csv', col_names = TRUE)

