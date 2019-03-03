source('Custom Functions work in progress.R')

# custom fucntion that downloads and install library if you don't have them, loads it if you do
ImportLib(tidyverse)

# remove OTPT claims associated with ED visits, and duplicate claim numbers. Creates claimsCleanSmall.RDS
createClaimsSmall()

claims <- read_rds('Data/claimsCleanSmall.RDS')

# add lead service type to claims. Creates claim_lead_age_group.rds
transform_claims(claims)

# turn diagnosis code into body_system category or css_category. Creates 5 temp rds files
# body_system for just body_system, css for just css_cateory or both
transform_codes(claims, 'both')

# free up some space for combining data
rm(claims)

# Creates claims_body_system_full.rds and claim_css_category.rds Removes temp files
# body_system for just body_system, css for just css_cateory or both
combine_data('both')

# Create chronic condition dataset
source('Chronic_Condition.R')

# read back in claims now that we don't need the space
claims <- read_rds("Data/claimsCleanSmall.RDS")

# load lead service dataset and body system
claims <- claims %>% 
  select(CLAIM_NUM,MRN_ALIAS, EPISODE_SEQ, MEMBER_SEX, SERVICE_TYPE)
lead<- read_rds('Data/claim_lead_age_group.rds')
body_system <- read_rds('Data/claim_body_system_full.RDS')
chronic <- read_rds('Data/claim_chronic_body_system_full.RDS')

# join together
full <- claims %>% 
  left_join(lead, by =c('CLAIM_NUM','MRN_ALIAS')) %>% 
  left_join(body_system, by = 'CLAIM_NUM') %>% 
  left_join(chronic, by = c('CLAIM_NUM', 'MRN_ALIAS'))

full <- full %>% 
  select(MRN_ALIAS, MEMBER_SEX, AGE_GROUP, starts_with('BS'), CLAIM_SEQ, EPISODE_SEQ, LEAD_EPISODE, SERVICE_TYPE,
         NEXT_SERVICE, YEAR, LEAD_YEAR, everything())

rm(claims,body_system, lead, chronic)
gc()

# save
saveRDS(full, 'Data/claims_body_as_code_with_lead_service.rds')

# now remove the code columns and join ccs codes instead, this creates a second data set run models on
# because ccs is from VP it might be more accurate but does include many more factor levels
full <- full %>% 
  select(-starts_with('CODE_'))

ccs <- read_rds('Data/claim_ccs_category_full.rds')

full <- full %>% 
  left_join(ccs, by = 'CLAIM_NUM')

saveRDS(full, 'Data/claims_ccs_as_code_with_lead_service.rds')
