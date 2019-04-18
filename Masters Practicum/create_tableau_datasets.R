library(tidyverse)
library(scales)
source('Custom Functions.R')

claims<- read_csv("Data/claims_ccs_as_code_with_lead_service.csv", 
                  col_types = cols(Asthma = col_number(), 
                                   CLAIM_SEQ = col_number(), COPD = col_number(), 
                                   Diabetes = col_number(), HeartFailure = col_number(), 
                                   Hypertension = col_number(), MoodDisorder = col_number(), 
                                   Psychoses = col_number(),Dementia = col_number(), 
                                   Epilepsy = col_number(), ESRD = col_number(), IHD = col_number(),
                                   SubstanceAbuse = col_number(),TIA = col_number()))

small <- claims %>% 
  select(MRN_ALIAS,CLAIM_NUM,SERVICE_TYPE, MEMBER_SEX, AGE_GROUP, Asthma, BrainInjury,
         COPD, Dementia,Diabetes, Epilepsy, ESRD,HeartFailure,Hypertension, IHD,
         MoodDisorder,Psychoses,SubstanceAbuse,TIA, YEAR)

small[6:19] <- lapply(small[6:19], as.numeric)

df <- read_rds('Data/claimscleanSmall.RDS')

df <- df %>% 
  select(MRN_ALIAS,APPROVED_AMT)

df$MRN_ALIAS <- as.character(df$MRN_ALIAS)
small$MRN_ALIAS <- as.character(small$MRN_ALIAS)

rm(claims)

cc <- small %>% 
  select(-MRN_ALIAS) %>% 
  group_by(SERVICE_TYPE, AGE_GROUP, MEMBER_SEX, YEAR) %>% 
  summarise_all(sum, na.rm = TRUE)

members <- small %>% 
  select(-SERVICE_TYPE, -YEAR, -MEMBER_SEX, -AGE_GROUP) %>% 
  group_by(MRN_ALIAS) %>% 
  summarise_all(max, na.rm = TRUE)

members2 <- df %>% 
  group_by(MRN_ALIAS) %>% 
  summarise(Amount_Approved = sum(APPROVED_AMT)) 

members3 <- members %>% 
  left_join(members2)

cc_long <- cc %>% 
  gather(Chronic_Condition, Number_of_Visits,Asthma:TIA)

members[members == '-Inf'] <- 0


members_long <- members3 %>% 
gather(Chronic_Condition, Number_of_Members, Asthma:SubstanceAbuse) %>% 
  group_by(Chronic_Condition) %>% 
  summarize(Number_of_Members = sum(Number_of_Members, na.rm = TRUE))


write_csv(cc_long, 'chronic_condition.csv')
write_csv(members3, 'members_costs.csv')
